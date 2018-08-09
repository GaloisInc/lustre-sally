{-# Language OverloadedStrings, PatternSynonyms #-}
-- | Deals with translation to and from Sally (using its MCMT output).
module Sally
  ( translateTS
  , translateQuery
  , SExpr
  , showsSExpr, ppSExpr
  , sally
  , readSallyResults, readSallyResult, SallyResult, Trace(..), VarVals
  ) where

import Data.List(unfoldr)
import Data.Char(isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad(zipWithM)
import Control.Exception(finally)
import System.IO(hPutStrLn,hClose,openTempFile)
import System.Directory(removeFile,getTemporaryDirectory)
import System.Process(readProcess)
import qualified SimpleSMT as SMT
import SimpleSMT (SExpr(..), showsSExpr, ppSExpr, sexprToVal, readSExpr)

import TransitionSystem as TS
import LSPanic(panic)

data QualState = ImplicitState | ExplicitState

sysName, sysStateType :: SExpr
sysName       = SMT.const "TS"
sysStateType  = SMT.const "S"

translateTS :: TransSystem -> [SExpr]
translateTS ts
  | Prelude.not (validTS ts) =
      panic "translate" [ "Not a valid transition system.", show ts ]
  | otherwise = [ declareStateType ts, declareTransSys ts ]

declareTransSys :: TransSystem -> SExpr
declareTransSys ts =
  SMT.fun "define-transition-system"
      [ sysName
      , sysStateType
      , toSallyExpr ImplicitState (tsInit ts)
      , toSallyExpr ExplicitState (tsTrans ts)
      ]

translateQuery :: TransSystem -> Expr -> SExpr
translateQuery ts q
  | validStatePred ts q = toSallyQuery q
  | otherwise           = panic "translateQuery" [ "Invalid query", show q ]


toSallyQuery :: Expr -> SExpr
toSallyQuery e = SMT.fun "query" [ sysName, toSallyExpr ImplicitState e ]

declareStateType :: TransSystem -> SExpr
declareStateType ts =
  SMT.fun "define-state-type" [ sysStateType, decls tsVars, decls tsInputs ]
  where
  decls f    = List (map decl (Map.toList (f ts)))
  decl (x,t) = List [ toSallyName x, toSallyType t ]

toSallyName :: Name -> SExpr
toSallyName (Name x) = SMT.const (Text.unpack x)

toSallyQualName :: VarNameSpace -> Name -> SExpr
toSallyQualName ns (Name x) =
  case ns of
    InCurState  -> pref "state"
    InNextState -> pref "next"
    FromInput   -> pref "input"
  where
  pref p = SMT.const (p ++ "." ++ Text.unpack x)

toSallyType :: Type -> SExpr
toSallyType ty =
  case ty of
    TInteger -> SMT.tInt
    TReal    -> SMT.tReal
    TBool    -> SMT.tBool

toSallyExpr :: QualState -> Expr -> SExpr
toSallyExpr qs expr =
  case expr of
    ns ::: x ->
      case qs of
        ExplicitState -> toSallyQualName ns x
        ImplicitState -> toSallyName x

    TS.Int x  -> SMT.int x
    TS.Real x -> SMT.real x

    x :+: y   -> SMT.add (toSallyExpr qs x) (toSallyExpr qs y)
    x :-: y   -> SMT.sub (toSallyExpr qs x) (toSallyExpr qs y)
    x :*: y   -> SMT.mul (toSallyExpr qs x) (toSallyExpr qs y)
    x :/: y   -> SMT.realDiv (toSallyExpr qs x) (toSallyExpr qs y)
    Div x y   -> SMT.div (toSallyExpr qs x) (toSallyExpr qs y)
    Mod x y   -> SMT.mod (toSallyExpr qs x) (toSallyExpr qs y)

    TS.Bool x -> SMT.bool x
    Not p     -> SMT.not (toSallyExpr qs p)
    _ :&&: _  -> SMT.andMany (flatAnd expr [])
    _ :||: _  -> SMT.orMany  (flatOr expr [])

    x :==: y  -> SMT.eq  (toSallyExpr qs x) (toSallyExpr qs y)
    x :<:  y  -> SMT.lt  (toSallyExpr qs x) (toSallyExpr qs y)
    x :<=: y  -> SMT.leq (toSallyExpr qs x) (toSallyExpr qs y)

    ITE x y z -> SMT.ite (toSallyExpr qs x)
                         (toSallyExpr qs y) (toSallyExpr qs z)

    _ -> panic "toSallyExpr" [ "Unexpected expression", show expr ]

  where
  flatAnd e more =
    case e of
      p :&&: q -> flatAnd p (flatAnd q more)
      _        -> toSallyExpr qs e : more

  flatOr e more =
    case e of
      p :||: q -> flatOr p (flatOr q more)
      _        -> toSallyExpr qs e : more


-- | Result of running sally on a query.
-- To produce traces, we need
data SallyResult t  = Valid
                    | Unknown
                    | Invalid !t
                    deriving (Eq,Show)

instance Functor SallyResult where
  fmap f res =
    case res of
      Valid -> Valid
      Unknown -> Unknown
      Invalid t -> Invalid (f t)

instance Foldable SallyResult where
  foldMap f res =
    case res of
      Valid -> mempty
      Unknown -> mempty
      Invalid t -> f t

instance Traversable SallyResult where
  traverse f res =
    case res of
      Valid -> pure Valid
      Unknown -> pure Unknown
      Invalid t -> Invalid <$> f t


type TSSallyResult = SallyResult TSTrace

type Perhaps = Either String

perhaps :: String -> Maybe a -> Perhaps a
perhaps x mb = case mb of
                 Nothing -> Left x
                 Just a  -> Right a


-- | Parse-out multiple results from Sally.
-- Assumes that @--show-trace@ was given.
readSallyResults :: TransSystem -> String -> Perhaps [TSSallyResult]
readSallyResults ts inp =
  case dropWhile isSpace inp of
    []   -> pure []
    inp1 ->
      do (r,inp2) <- readSallyResult ts inp1
         rs       <- readSallyResults ts inp2
         return (r:rs)

-- | Parse-out the result from Sally. Assumes that @--show-trace@ was given.
readSallyResult :: TransSystem -> String -> Perhaps (TSSallyResult, String)
readSallyResult ts xs =
  case break (== '\n') xs of
    ("valid",rest)    -> return (Valid, rest)
    ("unknown",rest)  -> return (Sally.Unknown, rest)
    ("invalid",_:ys)  -> do (t,rest) <- readTrace ts ys
                            pure (Invalid t, rest)
    _                 -> Left ("Unexpected response: " ++ xs)

-- | Parse a trace for the given system.
readTrace :: TransSystem -> String -> Perhaps (TSTrace, String)
readTrace ts inp =
  do (s, rest) <- perhaps "Failed to parse S-expression" (readSExpr inp)
     t <- parseTrace ts s
     return (t, rest)

-- | Parse an entire trace.
parseTrace :: TransSystem -> SExpr -> Perhaps TSTrace
parseTrace ts expr =
  case expr of
    List (Atom "trace" : s0 : steps) ->
        do s  <- parseState ts s0
           ss <- parseSteps ts steps
           return $! Trace { traceStart = s, traceSteps = ss }
    _ -> Left $ unlines [ "Expected 'trace'"
                        , "Got:"
                        , SMT.ppSExpr expr ""
                        ]

-- | Parse some values, either a steate or some inputs.
parseVals :: TransSystem -> String -> SExpr -> Perhaps VarVals
parseVals ts what s =
  case s of
    List (Atom ty : binds)
      | ty == what
      , Just tys <- case ty of
                      "state" -> Just (tsVars ts)
                      "input" -> Just (tsInputs ts)
                      _       -> Nothing
       -> Map.fromList <$> mapM (parseBind tys) binds
    _  -> Left $ unlines [ "Expected: " ++ show what
                         , "Got:"
                         , ppSExpr s ""
                         ]

-- | Parse a state definition.
parseState :: TransSystem -> SExpr -> Perhaps VarVals
parseState ts = parseVals ts "state"

{- | Parse the steps for a trace.
Note that we expect that there will be always an `input` entry,
even if there are no inputs: it would appear that Sally distinguishes
between systems with an empty slits of inputs, and ones where there
was no inputs.  Since we always generate a list of inputs, even when
empty, we always expect an input in the trace. -}
parseSteps :: TransSystem -> [SExpr] -> Perhaps [(VarVals,VarVals)]
parseSteps ts steps =
    do vars <- zipWithM (parseVals ts) (cycle ["input","state"]) steps
       sequence (unfoldr takeStep vars)
  where
  takeStep xs = case xs of
                  []           -> Nothing
                  [_]          -> Just (Left "Odd results",[])
                  a : b : more -> Just (Right (a,b),more)


-- | Parse a variable binding, assuming the given variable types.
parseBind :: Map Name Type -> SExpr -> Perhaps (Name,TS.Value)
parseBind tys s =
  case s of
    List [ Atom x, v ] ->
      do let nm = Name $ Text.pack
                       $ case x of
                           '|' : more | not (null more) && last more == '|' ->
                                        init more
                           _ -> x
         ty <- perhaps ("Undefined variable: " ++ show x) (Map.lookup nm tys)
         val <- parseValue ty v
         return (nm, val)
    _ -> Left "Invalid binding"

-- | Parse a value of the given type.
parseValue :: Type -> SExpr -> Perhaps TS.Value
parseValue ty s =
  case (ty,sexprToVal s) of
    (TInteger, SMT.Int x)   -> return (VInt x)
    (TReal,    SMT.Int x)   -> return (VReal (fromIntegral x))
    (TReal,    SMT.Real x)  -> return (VReal x)
    (TBool,    SMT.Bool x)  -> return (VBool x)
    _                       -> Left $ unlines [ "Invalid value"
                                              , "*** Type: " ++ show ty
                                              , "*** Expr: " ++ showsSExpr s ""
                                              ]


-- | Run sally with the given options, on the given output.
-- This creates a temporary file, saves the input, and runs sally on it.
sally :: FilePath -> [String] -> String -> IO String
sally exe opts inp =
  do tmp <- getTemporaryDirectory
     (path,h) <- openTempFile tmp "sallyXXX.mcmt"
     do hPutStrLn h inp
        hClose h
        readProcess exe (opts ++ [path]) ""
       `finally` removeFile path





{-# Language OverloadedStrings, PatternSynonyms #-}
module Sally
  ( translateTS
  , translateQuery
  , SExpr
  , showsSExpr
  , sally
  , readSallyResult, SallyResult, Trace(..), VarVals
  ) where

import Data.List(unfoldr)
import Data.Char(isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Control.Monad(zipWithM,unless)
import Control.Exception(finally)
import System.IO(hPutStrLn,hClose,openTempFile)
import System.Directory(removeFile,getTemporaryDirectory)
import System.Process(readProcess)
import SimpleSMT as SMT

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
  fun "define-transition-system"
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
toSallyQuery e = fun "query" [ sysName, toSallyExpr ImplicitState e ]

declareStateType :: TransSystem -> SExpr
declareStateType ts =
  fun "define-state-type" [ sysStateType, decls tsVars, decls tsInputs ]
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
    TInteger -> tInt
    TReal    -> tReal
    TBool    -> tBool

toSallyExpr :: QualState -> Expr -> SExpr
toSallyExpr qs expr =
  case expr of
    ns ::: x ->
      case qs of
        ExplicitState -> toSallyQualName ns x
        ImplicitState -> toSallyName x

    TS.Int x  -> int x
    TS.Real x -> real x

    x :+: y   -> add (toSallyExpr qs x) (toSallyExpr qs y)
    x :-: y   -> sub (toSallyExpr qs x) (toSallyExpr qs y)
    x :*: y   -> mul (toSallyExpr qs x) (toSallyExpr qs y)
    x :/: y   -> realDiv (toSallyExpr qs x) (toSallyExpr qs y)
    Div x y   -> SMT.div (toSallyExpr qs x) (toSallyExpr qs y)
    Mod x y   -> SMT.mod (toSallyExpr qs x) (toSallyExpr qs y)

    TS.Bool x -> bool x
    Not p     -> SMT.not (toSallyExpr qs p)
    _ :&&: _  -> SMT.andMany (flatAnd expr [])
    _ :||: _  -> SMT.orMany  (flatOr expr [])

    x :==: y  -> eq  (toSallyExpr qs x) (toSallyExpr qs y)
    x :<:  y  -> lt  (toSallyExpr qs x) (toSallyExpr qs y)
    x :<=: y  -> leq (toSallyExpr qs x) (toSallyExpr qs y)

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

type VarVals  = Map Name TS.Value
data Trace    = Trace { traceStart :: !VarVals             -- ^ Initial state
                      , traceSteps :: ![(VarVals,VarVals)] -- ^ (Input,NewState)
                      } deriving (Eq,Show)


-- | Result of running sally on a query.
-- To produce traces, we need
data SallyResult  = Valid
                  | Unknown
                  | Invalid !Trace
                    deriving (Eq,Show)

type Perhaps = Either String

perhaps :: String -> Maybe a -> Perhaps a
perhaps x mb = case mb of
                 Nothing -> Left x
                 Just a  -> Right a

-- | Parse-out the result from Sally. Assumes that @--show-trace@ was given.
readSallyResult :: TransSystem -> String -> Perhaps SallyResult
readSallyResult ts xs =
  case break (== '\n') xs of
    ("valid",_)       -> return Valid
    ("unknown",_)     -> return Sally.Unknown
    ("invalid",_:ys)  -> Invalid <$> readTrace ts ys
    _                 -> Left ("Unexpected response: " ++ xs)

-- | Parse a trace for the given system.
readTrace :: TransSystem -> String -> Perhaps Trace
readTrace ts t =
  do (s, rest) <- perhaps "Failed to parse S-expression" (readSExpr t)
     unless (all isSpace rest) (Left "Left-overs at the end.")
     parseTrace ts s

-- | Parse an entire trace.
parseTrace :: TransSystem -> SExpr -> Perhaps Trace
parseTrace ts expr =
  case expr of
    List (Atom "trace" : s0 : steps) ->
        do s  <- parseState ts s0
           ss <- parseSteps ts steps
           return $! Trace { traceStart = s, traceSteps = ss }
    _ -> Left "Expected 'trace'"

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
    _  -> Left ("Expected: " ++ show what)

-- | Parse a state definition.
parseState :: TransSystem -> SExpr -> Perhaps VarVals
parseState ts = parseVals ts "state"

-- | Parse the steps for a trace.
parseSteps :: TransSystem -> [SExpr] -> Perhaps [(VarVals,VarVals)]
parseSteps ts steps
  | Map.null (tsInputs ts) =
    do vars <- mapM (parseState ts) steps
       return [ (Map.empty, s) | s <- vars ]

  | otherwise =
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
      do let nm = Name (Text.pack x)
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



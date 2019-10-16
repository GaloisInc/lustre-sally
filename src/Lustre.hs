{-# Language OverloadedStrings, PatternSynonyms #-}
-- | Translate Core Lustre to a transtion system.
-- In this translation there are no explicit `nil` values, instead
-- such values are modelled as arbitrary unconstrained values of
-- the appropriate type.
module Lustre (transNode, transProp, importTrace, LTrace) where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe(mapMaybe)
import           Data.Char(isAscii,isAlpha,isDigit)

import qualified TransitionSystem as TS


import Language.Lustre.Name
import Language.Lustre.Core
import qualified Language.Lustre.Semantics.Core as L
import Language.Lustre.Pretty(showPP)
import LSPanic

transNode :: Node -> (TS.TransSystem, [(Label,TS.Expr)])
transNode n = (ts, map mkProp (nShows n))
  where
  mkProp (x,p) = (x, transProp qns TS.InCurState p)

  env = nodeEnv n
  qns = identVariants n

  ts = TS.TransSystem
         { TS.tsVars    = Map.unions
                            (inVars : otherVars
                                    : map (declareVar qns) (nAbstract n)
                                   ++ map (declareEqn qns) (nAllEqns n))
         , TS.tsInputs  = inVars
         , TS.tsInit    = initNode qns n
         , TS.tsTrans   = stepNode qns env n
         }

  inVars    = Map.unions (map (declareVar qns) (nInputs n))
  otherVars = declareVarInitializing

nAllEqns :: Node -> [Eqn]
nAllEqns = concatMap grpEqns . nEqns


type Env = Map CoreName CType
type Val = TS.Expr
type TVal = (Val,Type)
type QNames = Map CoreName Int

-- | Literals are not nil.
valLit :: Literal -> Val
valLit lit = case lit of
               Int n  -> TS.Int n
               Bool b -> TS.Bool b
               Real r -> TS.Real r


idText :: QNames -> CoreName -> Text
idText qs i =
  case Map.lookup i qs of
    Just 0 -> txt
    Just n -> txt <> ":" <> Text.pack (show n)
    Nothing -> panic "idText" ["Missing entry for identifier:"
                              , "*** Identifier: " ++ show i ]
    where txt = coreNameTextName i

-- | The logical variable for the ordinary value.
valName :: QNames -> CoreName -> TS.Name
valName qns i
  | Text.all simp x = TS.Name x
  | otherwise       = TS.Name ("|" <> x <> "|")
  where
  simp a = isAscii a && (isAlpha a || isDigit a)
  x      = idText qns i

-- | For variables defined by @a -> b@, keeps track if we are in the @a@
-- (value @true@) or in the @b@ part (value @false@).
initName :: QNames -> CoreName -> TS.Name
initName qs i = TS.Name ("|" <> idText qs i <> ":init|")

-- | Translate an atom, by using the given name-space for variables.
valAtom :: QNames -> Env -> TS.VarNameSpace -> Atom -> Val
valAtom qs env ns atom =
  case atom of
    Lit l _   -> valLit l
    Var a     -> ns TS.::: valName qs a
    Prim f as -> primFun f (map evT as)
      where evT a = (valAtom qs env ns a, typeOfCType (typeOf env a))

-- | A boolean variable which is true in the very first state,
-- beofre we've received any inputs, and false after-wards..
-- We use it to avoid checking queries in the very first state
varInitializing :: TS.Name
varInitializing = TS.Name "|gal-initializing|"



{- Note: Handling Inputs
   =====================

In Sally, inputs live in a separate name space, and are an input to
the transition relation.  So to translate something like @pre x@ where
@x@ is an input, we need a new local variable that will store the old
value for @x@.  To make things uniform, we introduce one local variable
per input: in this way, accessing variables is always done the same way
(@pre x@ is @current.x@ and @x@ is @next.x@).
-}


--------------------------------------------------------------------------------
-- Syntactic sugar for making TS expressions.

true, false :: TS.Expr
true  = TS.Bool True
false = TS.Bool False

-- | And tigether multiple boolean expressions.
ands :: [TS.Expr] -> TS.Expr
ands as =
  case as of
    [] -> true
    _  -> foldr1 (TS.:&&:) as



-- | Equations asserting that a varible from some namespace has the given value.
setVals :: QNames -> TS.VarNameSpace -> CoreName -> Val -> TS.Expr
setVals qns ns x v = ns TS.::: (valName qns x) TS.:==: v

-- | Properties get translated into queries.
-- We are not interested in validating the initial state, which is
-- full of indetermined values.
transProp :: QNames -> TS.VarNameSpace -> CoreName -> TS.Expr
transProp qns ns i = (ns TS.::: varInitializing) TS.:||: transBool qns ns i

-- | Properties get translated into queries. @nil@ is treated as @False@.
-- We are not interested in validating the initial state, which is
-- full of @nil@.
transBool :: QNames -> TS.VarNameSpace -> CoreName -> TS.Expr
transBool qns ns i = ns TS.::: valName qns i



--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- Declaring variables
transType :: Type -> TS.Type
transType ty =
  case ty of
    TInt  -> TS.TInteger
    TBool -> TS.TBool
    TReal -> TS.TReal

-- | Declare all parts of a variable.
-- See "NOTE: Translating Variables"
declareVar :: QNames -> Binder -> Map TS.Name TS.Type
declareVar qns (x ::: t `On` _) = Map.singleton (valName qns x) (transType t)

-- | Local variables.
declareEqn :: QNames -> Eqn -> Map TS.Name TS.Type
declareEqn qns (x@(v ::: _) := e) =
  case e of
    _ :-> _ -> Map.insert (initName qns v) TS.TBool mp
    _ -> mp
  where mp = declareVar qns x


-- | Keep track if we are in the initial state.
declareVarInitializing :: Map TS.Name TS.Type
declareVarInitializing = Map.singleton varInitializing TS.TBool
--------------------------------------------------------------------------------



-- | Initial state for a node. All variable start off as indeterminate.
initNode :: QNames -> Node -> TS.Expr
initNode qns n = ands (setInit : mapMaybe initS (nAllEqns n))
  where
  initS ((v ::: _) := e) =
    case e of
      _ :-> _ -> Just ((TS.InCurState TS.::: initName qns v) TS.:==: true)
      _ -> Nothing

  setInit = TS.InCurState TS.::: varInitializing TS.:==: true

stepNode :: QNames -> Env -> Node -> TS.Expr
stepNode qns env n =
  ands $ ((TS.InNextState TS.::: varInitializing) TS.:==: false) :
         map (stepInput qns env) (nInputs n) ++
         map (transBool qns TS.InNextState . snd) (nAssuming n) ++
         map (stepEqn qns env) (nAllEqns n)

-- XXX: clocks?
stepInput :: QNames -> Env -> Binder -> TS.Expr
stepInput qns env (x ::: _) = setVals qns TS.InNextState x a
  where a = valAtom qns env TS.FromInput (Var x)

stepEqn :: QNames -> Env -> Eqn -> TS.Expr
stepEqn qns env (x ::: _ `On` c := expr) =
  case expr of
    Atom a      -> guarded (new a)
    Current a   -> new a
    Pre a       -> guarded (old a)
    a `When` _  -> guarded (new a)

    a :-> b     ->
      case clockTowerOf c of
        [] -> clockYes
        g  -> TS.ITE (ands g) clockYes hold
      where
      hold = old (Var x) TS.:&&: ivar TS.InNextState TS.:==: ivar TS.InCurState
      ivar n = n TS.::: initName qns x
      clockYes = TS.ITE (ivar TS.InCurState) (new a) (new b) TS.:&&:
                 ivar TS.InNextState TS.:==: false

    Merge a ifT ifF -> guarded (TS.ITE a' (new ifT) (new ifF))
      where a' = atom a


  where
  atom    = valAtom qns env TS.InNextState
  old     = letVars . valAtom qns env TS.InCurState
  new     = letVars . atom
  letVars = setVals qns TS.InNextState x

  clockTowerOf cl =
    case cl of
      BaseClock -> []
      WhenTrue a -> atom a : clockTowerOf (clockOfCType (typeOf env a))


  guarded e = case clockTowerOf c of
                [] -> e              -- base clock
                g  -> TS.ITE (ands g) e (old (Var x))


-- | Translation of primitive functions.
primFun :: Op -> [TVal] -> Val
primFun op as =
  case (op,as) of
    (Neg, [(a,_)])    -> TS.Neg a
    (Not, [(a,_)])    -> TS.Not a

    (And, _)          -> bool2 (TS.:&&:)
    (Or,  _)          -> bool2 (TS.:||:)
    (Xor, _)          -> bool2 (TS.:/=:)
    (Implies, _)      -> bool2 (TS.:=>:)

    (Add, [(a,_),(b,_)])      -> a TS.:+: b
    (Sub, [(a,_),(b,_)])      -> a TS.:-: b
    (Mul, [(a,_),(b,_)])      -> a TS.:*: b
    (Mod, [(a,_),(b,_)])      -> TS.Mod a b
    (Div, [(a,ta),(b,tb)])    ->
      case (ta, tb) of
        (TInt,TInt)           -> TS.Div a b
        (TReal,TReal)         -> a TS.:/: b
        _ -> panic "primFun(Div)" ["Don't know how to divide"
                                  , showPP ta, showPP tb ]

    (Eq,  _)          -> rel (TS.:==:)
    (Neq, _)          -> rel (TS.:/=:)
    (Lt,  _)          -> rel (TS.:<:)
    (Leq, _)          -> rel (TS.:<=:)
    (Gt,  _)          -> rel (TS.:>:)
    (Geq, _)          -> rel (TS.:>=:)

    (IntCast, [(a,_)]) -> TS.ToIntTrunc a
    (FloorCast, [(a,_)]) -> TS.ToIntFloor a
    (RealCast, [(a,_)])-> TS.ToReal a

    (AtMostOne, _)    -> mkAtMostOne
    (Nor, _)          -> case as of
                           []  -> true
                           _   -> TS.Not (foldr1 (TS.:||:) (map fst as))

    (ITE, [(a,_),(b,_),(c,_)])    -> TS.ITE a b c

    _ -> error ("XXX: " ++ show op)

  where
  bool2 f = case as of
              [(a,_),(b,_)] -> f a b
              _ -> panic "primFun" ["Invalid bool2"]

  rel f = case as of
            [(a,_),(b,_)] -> f a b
            _ -> panic "primFun" ["Invalid rel"]

  mkAtMostOne = case as of
                  [] -> true
                  _  -> atMostOneVal as

  norVal xs = case xs of
                [] -> true
                _  -> TS.Not (foldr1 (TS.:||:) (map fst xs))

  atMostOneVal vs =
    case vs of
      []     -> true
      [_]    -> true
      [(a,_),(b,_)]  -> a TS.:=>: TS.Not b
      (a,_) : bs -> TS.ITE a (norVal bs) (atMostOneVal bs)



--------------------------------------------------------------------------------
-- Importing of Traces


type ImportError = String

-- | Fail to import something
importError :: [String] -> Either ImportError a
importError = Left . unlines

-- | Import a Lustre identifier from the given assignment computed by Sally.
-- See "Translating Variables" for details of what's going on here.
importVar :: QNames -> TS.VarVals -> CoreName -> Either ImportError L.Value
importVar qns st i =
  case Map.lookup vaName st of
    Just v -> pure $ case v of
                       TS.VInt x  -> L.VInt x
                       TS.VBool x -> L.VBool x
                       TS.VReal x -> L.VReal x
    Nothing -> missing vaName
  where
  vaName = valName qns i

  missing x = importError [ "[bug] Missing assignment"
                          , "*** Variable: " ++ show x
                          ]

-- | Import a bunch of core Lustre identifiers from a state.
importVars :: QNames -> Set CoreName -> TS.VarVals ->
                                          Either ImportError (Map CoreName L.Value)
importVars qns vars st =
  do let is = Set.toList vars
     steps <- mapM (importVar qns st) is
     pure (Map.fromList (zip is steps))


importState ::
  QNames -> Node -> TS.VarVals -> Either ImportError (Map CoreName L.Value)
importState qns n =
  importVars qns $ Set.fromList
                 $ [ x | x ::: _ <- nInputs n ] ++
                   {- Inputs are shadowed in the state -}
                   [ x | x ::: _ := _ <- nAllEqns n ]

importInputs ::
  QNames -> Node -> TS.VarVals -> Either ImportError (Map CoreName L.Value)
importInputs qns n = importVars qns $ Set.fromList [ x | x ::: _ <- nInputs n ]

type LTrace = TS.Trace {-state-} (Map CoreName L.Value)
                       {-inputs-}(Map CoreName L.Value)

importTrace :: Node -> TS.TSTrace -> Either ImportError LTrace
importTrace n tr =
  case tr of
    TS.Trace { TS.traceStart = start, TS.traceSteps = steps } ->
      do start1 <- importState qns n start
         steps1 <- mapM impStep steps
         pure TS.Trace { TS.traceStart = start1, TS.traceSteps = steps1 }
  where
  qns = identVariants n
  impStep (i,s) =
    do i1 <- importInputs qns n i
       s1 <- importState qns n s
       return (i1,s1)





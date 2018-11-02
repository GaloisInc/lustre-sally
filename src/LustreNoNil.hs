{-# Language OverloadedStrings, PatternSynonyms #-}
-- | Translate Core Lustre to a transtion system.
-- In this translation there are no explicit `nil` values, instead
-- such values are modelled as arbitrary unconstrained values of
-- the appropriate type.
module LustreNoNil (transNode, transProp, importTrace, LTrace) where

import           Data.Text(Text)
import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe(mapMaybe)
import           Data.Char(isAscii,isAlpha,isDigit)

import qualified TransitionSystem as TS


import Language.Lustre.Core
import qualified Language.Lustre.Semantics.Value as L

transNode :: Node -> (TS.TransSystem, [(Text,TS.Expr)])
transNode n = (ts, [(x, transProp TS.InCurState p) | (x,p) <- nShows n])
  where
  ts = TS.TransSystem
         { TS.tsVars    = Map.unions (inVars : otherVars :map declareEqn (nEqns n))
         , TS.tsInputs  = inVars
         , TS.tsInit    = initNode n
         , TS.tsTrans   = stepNode n
         }

  inVars    = Map.unions (map declareVar (nInputs n))
  otherVars = declareVarInitializing



type Val = TS.Expr

-- | Literals are not nil.
valLit :: Literal -> Val
valLit lit = case lit of
               Int n  -> TS.Int n
               Bool b -> TS.Bool b
               Real r -> TS.Real r

-- | The logical variable for the ordinary value.
valName :: Ident -> TS.Name
valName (Ident x)
  | Text.all simp x = TS.Name x
  | otherwise       = TS.Name ("|" <> x <> "|")
  where simp a = isAscii a && (isAlpha a || isDigit a)

-- | For variables defined by @a -> b@, keeps track if we are in the @a@
-- (value @true@) or in the @b@ part (value @false@).
initName :: Ident -> TS.Name
initName (Ident x) = TS.Name ("|" <> x <> "-init|")

-- | Translate an atom, by using the given name-space for variables.
valAtom :: TS.VarNameSpace -> Atom -> Val
valAtom ns atom =
  case atom of
    Lit l     -> valLit l
    Var a     -> ns TS.::: valName a
    Prim f as -> primFun f (map (valAtom ns) as)

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
setVals :: TS.VarNameSpace -> Ident -> Val -> TS.Expr
setVals ns x v = ns TS.::: (valName x) TS.:==: v

-- | Properties get translated into queries.
-- We are not interested in validating the initial state, which is
-- full of indetermined values.
transProp :: TS.VarNameSpace -> Ident -> TS.Expr
transProp ns i = (ns TS.::: varInitializing) TS.:||: transBool ns i

-- | Properties get translated into queries. @nil@ is treated as @False@.
-- We are not interested in validating the initial state, which is
-- full of @nil@.
transBool :: TS.VarNameSpace -> Ident -> TS.Expr
transBool ns i = ns TS.::: valName i



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
declareVar :: Binder -> Map TS.Name TS.Type
declareVar (x ::: t `On` _) = Map.singleton (valName x) (transType t)

-- | Local variables.
declareEqn :: Eqn -> Map TS.Name TS.Type
declareEqn (x@(v ::: _) := e) = case e of
                        _ :-> _ -> Map.insert (initName v) TS.TBool mp
                        _ -> mp
  where mp = declareVar x


-- | Keep track if we are in the initial state.
declareVarInitializing :: Map TS.Name TS.Type
declareVarInitializing = Map.singleton varInitializing TS.TBool
--------------------------------------------------------------------------------



-- | Initial state for a node. All variable start off as indeterminate.
initNode :: Node -> TS.Expr
initNode n = ands (setInit : mapMaybe initS (nEqns n))
  where
  initS ((v ::: _) := e) =
    case e of
      _ :-> _ -> Just ((TS.InCurState TS.::: initName v) TS.:==: true)
      _ -> Nothing

  setInit = TS.InCurState TS.::: varInitializing TS.:==: true

stepNode :: Node -> TS.Expr
stepNode n =
  ands $ ((TS.InNextState TS.::: varInitializing) TS.:==: false) :
         map stepInput (nInputs n) ++
         map (transBool TS.InNextState . snd) (nAssuming n) ++
         map stepEqn (nEqns n)

-- XXX: clocks?
stepInput :: Binder -> TS.Expr
stepInput (x ::: _) = setVals TS.InNextState x (valAtom TS.FromInput (Var x))

stepEqn :: Eqn -> TS.Expr
stepEqn (x ::: _ `On` c := expr) =
  case expr of
    Atom a      -> new a
    Current a   -> new a
    Pre a       -> guarded (old a)
    a `When` _  -> guarded (new a)

    a :-> b     ->
      case clockOn of
        Nothing -> clockYes
        Just g ->
          TS.ITE g
            clockYes
            (old (Var x) TS.:&&: ivar TS.InNextState TS.:==: ivar TS.InCurState)
      where
      ivar n = n TS.::: initName x
      clockYes = (TS.ITE (ivar TS.InCurState) (new a) (new b)
                    TS.:&&: ivar TS.InNextState TS.:==: false)

    Merge a ifT ifF -> guarded (TS.ITE a' (new ifT) (new ifF))
      where a' = atom a


  where
  atom    = valAtom TS.InNextState
  old     = letVars . valAtom TS.InCurState
  new     = letVars . atom
  letVars = setVals TS.InNextState x

  guarded e = case clockOn of
                Nothing -> e
                Just g  -> TS.ITE g e (old (Var x))

  clockOn = case c of
              Lit (Bool True) -> Nothing -- base clocks
              _ -> Just (atom c)



-- | Translation of primitive functions.
primFun :: Op -> [Val] -> Val
primFun op as =
  case (op,as) of
    (Neg, [a])        -> TS.Neg a
    (Not, [a])        -> TS.Not a

    (And, [a,b])      -> a TS.:&&: b
    (Or,  [a,b])      -> a TS.:||: b
    (Xor,  [a,b])     -> a TS.:/=: b
    (Implies, [a,b])  -> a TS.:=>: b

    (Add, [a,b])      -> a TS.:+: b
    (Sub, [a,b])      -> a TS.:-: b
    (Mul, [a,b])      -> a TS.:*: b
    (Mod, [a,b])      -> TS.Mod a b
    (Div, [a,b])      -> TS.Div a b

    (Eq,  [a,b])      -> a TS.:==: b
    (Neq, [a,b])      -> a TS.:/=: b
    (Lt,  [a,b])      -> a TS.:<:  b
    (Leq, [a,b])      -> a TS.:<=: b
    (Gt,  [a,b])      -> a TS.:>:  b
    (Geq, [a,b])      -> a TS.:>=: b

    (IntCast, [a])    -> TS.ToInt a
    (RealCast, [a])   -> TS.ToReal a

    (AtMostOne, _)    -> mkAtMostOne
    (Nor, _)          -> case as of
                           []  -> true
                           _   -> TS.Not (foldr1 (TS.:||:) as)

    (ITE, [a,b,c])    -> TS.ITE a b c

    _ -> error ("XXX: " ++ show op)

  where
  mkAtMostOne = case as of
                  [] -> true
                  _  -> atMostOneVal as

  norVal xs = case xs of
                [] -> true
                _  -> TS.Not (foldr1 (TS.:||:) xs)

  atMostOneVal vs =
    case vs of
      []     -> true
      [_]    -> true
      [a,b]  -> a TS.:=>: TS.Not b
      a : bs -> TS.ITE a (norVal bs) (atMostOneVal bs)



--------------------------------------------------------------------------------
-- Importing of Traces


type ImportError = String

-- | Fail to import something
importError :: [String] -> Either ImportError a
importError = Left . unlines

-- | Import a Lustre identifier from the given assignment computed by Sally.
-- See "Translating Variables" for details of what's going on here.
importVar :: TS.VarVals -> Ident -> Either ImportError L.Value
importVar st i =
  case Map.lookup vaName st of
    Just v -> pure $ case v of
                       TS.VInt x  -> L.VInt x
                       TS.VBool x -> L.VBool x
                       TS.VReal x -> L.VReal x
    Nothing -> missing vaName
  where
  vaName = valName i

  missing x = importError [ "[bug] Missing assignment"
                          , "*** Variable: " ++ show x
                          ]

-- | Import a bunch of core Lustre identifiers from a state.
importVars :: Set Ident -> TS.VarVals -> Either ImportError (Map Ident L.Value)
importVars vars st =
  do let is = Set.toList vars
     steps <- mapM (importVar st) is
     pure (Map.fromList (zip is steps))

importState :: Node -> TS.VarVals -> Either ImportError (Map Ident L.Value)
importState n = importVars $ Set.fromList [ x | x ::: _ := _ <- nEqns n ]

importInputs :: Node -> TS.VarVals -> Either ImportError (Map Ident L.Value)
importInputs n = importVars $ Set.fromList [ x | x ::: _ <- nInputs n ]

type LTrace = TS.Trace (Map Ident L.Value) (Map Ident L.Value)

importTrace :: Node -> TS.TSTrace -> Either ImportError LTrace
importTrace n tr =
  case tr of
    TS.Trace { TS.traceStart = start, TS.traceSteps = steps } ->
      do start1 <- importState n start
         steps1 <- mapM impStep steps
         pure TS.Trace { TS.traceStart = start1, TS.traceSteps = steps1 }
  where
  impStep (i,s) =
    do i1 <- importInputs n i
       s1 <- importState n s
       return (i1,s1)





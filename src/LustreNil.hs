{-# Language OverloadedStrings, PatternSynonyms #-}
-- | Translate Core Lustre to a transtion system.
module LustreNil (transNode, transProp, importTrace) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Maybe(mapMaybe)

import qualified TransitionSystem as TS


import Language.Lustre.Core
import qualified Language.Lustre.Semantics.Value as L

import LSPanic

transNode :: Node -> (TS.TransSystem, [TS.Expr])
transNode n = (ts, map (transProp TS.InCurState) (nShows n))
  where
  ts = TS.TransSystem
         { TS.tsVars    = Map.unions (inVars : otherVars :map declareEqn (nEqns n))
         , TS.tsInputs  = inVars
         , TS.tsInit    = initNode n
         , TS.tsTrans   = stepNode n
         }

  inVars    = Map.unions (map declareVar (nInputs n))
  otherVars = declareVarInitializing



{- NOTE:  Translating Variables
   ============================

Uninitialized Lustre varaibles have the value `Nil`.  To keep track of that,
we use two logical variables for each Lustre variable:

  X     : T
  X_nil : Bool

If `X_nil` is `true`, then this value is nil and the value of `X` is
irrelevant.
-}


data Val = Val { vVal :: TS.Expr
                 -- ^ type T.  The "normal" value.

               , vNil :: TS.Expr
                 -- ^ type Bool.
                 -- Indicates if the value is nil.
                 -- If so, the "normal" value is ignored.
               }

-- | Literals are not nil.
valLit :: Literal -> Val
valLit lit = Val { vVal = case lit of
                            Int n  -> TS.Int n
                            Bool b -> TS.Bool b
                            Real r -> TS.Real r
                 , vNil = false
                 }

-- | The logical variable for the ordinary value.
valName :: Ident -> TS.Name
valName (Ident x) = TS.Name ("gal_" <> x)

-- | The logical variable keeping track if a value is nil.
nilName :: Ident -> TS.Name
nilName (Ident x) = TS.Name ("gal_" <> x <> "_nil")

-- | For variables defined by @a -> b@, keeps track if we are in the @a@
-- (value @false@) or in the @b@ part (value @true@).
initName :: Ident -> TS.Name
initName (Ident x) = TS.Name ("gal_" <> x <> "_init")

-- | Translate an atom, by using the given name-space for variables.
valAtom :: TS.VarNameSpace -> Atom -> Val
valAtom ns atom =
  case atom of
    Lit l -> valLit l
    Var a -> Val { vVal = ns TS.::: valName a
                 , vNil = ns TS.::: nilName a
                 }

-- | A boolean variable which is true in the very first state,
-- beofre we've received any inputs, and false after-wards..
-- We use it to avoid checking queries in the very first state
varInitializing :: TS.Name
varInitializing = TS.Name ("gal_initializing")



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
    [] -> TS.Bool True
    _  -> foldr1 (TS.:&&:) as



-- | Equations asserting that a varible from some namespace has the given value.
setVals :: TS.VarNameSpace -> Ident -> Val -> TS.Expr
setVals ns x v = var valName TS.:==: vVal v TS.:&&:
                 var nilName TS.:==: vNil v
  where
  var f = ns TS.::: f x

-- | Assert that a specific variable is @nil@.
setNil :: TS.VarNameSpace -> Ident -> TS.Expr
setNil ns x = ns TS.::: nilName x TS.:==: true

-- | Properties get translated into queries. @nil@ is treated as @False@.
-- We are not interested in validating the initial state, which is
-- full of @nil@.
transProp :: TS.VarNameSpace -> Ident -> TS.Expr
transProp ns i = (ns TS.::: varInitializing) TS.:||: transBool ns i

-- | Properties get translated into queries. @nil@ is treated as @False@.
-- We are not interested in validating the initial state, which is
-- full of @nil@.
transBool :: TS.VarNameSpace -> Ident -> TS.Expr
transBool ns i = TS.Not (v nilName) TS.:&&: v valName
  where
  v f = ns TS.::: f i




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
declareVar (x ::: t `On` _) =
  Map.fromList [ (valName x, transType t)
               , (nilName x, TS.TBool)
               ]

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



-- | Initial state for a node. All variable start off as @nil@.
initNode :: Node -> TS.Expr
initNode n = ands (setInit : mapMaybe initS (nEqns n) ++ map initB allVars)
  where
  allVars         = nInputs n ++ [ b | b := _ <- nEqns n ]
  initB (x ::: _) = setNil TS.InCurState x
  initS ((v ::: _) := e) =
    case e of
      _ :-> _ -> Just ((TS.InCurState TS.::: initName v) TS.:==: false)
      _ -> Nothing

  setInit = TS.InCurState TS.::: varInitializing TS.:==: true

stepNode :: Node -> TS.Expr
stepNode n =
  ands $ ((TS.InNextState TS.::: varInitializing) TS.:==: false) :
         map stepInput (nInputs n) ++
         map (transBool TS.InNextState) (nAssuming n) ++
         map stepEqn (nEqns n)

stepInput :: Binder -> TS.Expr
stepInput (x ::: _) = setVals TS.InNextState x (valAtom TS.FromInput (Var x))

stepEqn :: Eqn -> TS.Expr
stepEqn (x ::: _ `On` c := expr) =
  case expr of
    Atom a      -> new a
    Current a   -> new a
    Pre a       -> guarded (old a)
    a `When` _  -> guarded (new a)
    Prim f as   -> guarded (letVars (primFun f (map atom as)))

    a :-> b     ->
      case clockOn of
        Nothing -> clockYes
        Just g ->
          TS.ITE g
            clockYes
            (old (Var x) TS.:&&: ivar TS.InNextState TS.:==: ivar TS.InCurState)
      where
      ivar n = n TS.::: initName x
      clockYes = (TS.ITE (ivar TS.InCurState) (new b) (new a)
                    TS.:&&: ivar TS.InNextState TS.:==: true)

    Merge a ifT ifF -> guarded $
        TS.ITE (vNil a')
               (setNil TS.InNextState x)
               (TS.ITE (vVal a') (new ifT) (new ifF))
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
              _ -> let c' = atom c
                   in Just (TS.Not (vNil c') TS.:&&: vVal c')



-- | Translation of primitive functions.
primFun :: Op -> [Val] -> Val
primFun op as =
  case (op,as) of
    (Not, [a])   -> op1 (TS.Not) a

    (And, [a,b])      -> op2 (TS.:&&:) a b
    (Or,  [a,b])      -> op2 (TS.:||:) a b
    (Xor,  [a,b])     -> op2 (TS.:/=:) a b
    (Implies, [a,b])  -> op2 (TS.:=>:) a b

    (Add, [a,b])      -> op2 (TS.:+:) a b
    (Sub, [a,b])      -> op2 (TS.:-:) a b
    (Mul, [a,b])      -> op2 (TS.:*:) a b
    (Mod, [a,b])      -> op2 TS.Mod a b
    (Div, [a,b])      -> op2 TS.Div a b

    (Eq,  [a,b])      -> op2 (TS.:==:) a b
    (Neq, [a,b])      -> op2 (TS.:/=:) a b
    (Lt,  [a,b])      -> op2 (TS.:<:) a b
    (Leq, [a,b])      -> op2 (TS.:<=:) a b
    (Gt,  [a,b])      -> op2 (TS.:>:) a b
    (Geq, [a,b])      -> op2 (TS.:>=:) a b

    (IntCast, [a])    -> op1 TS.ToInt a
    (RealCast, [a])   -> op1 TS.ToReal a

    (AtMostOne, _)    -> mkAtMostOne
    (Nor, _)          -> case as of
                           []  -> valLit (Bool True)
                           _   -> op1 TS.Not (foldr1 (op2 (TS.:||:)) as)

    (ITE, [a,b,c]) -> Val { vVal = TS.ITE (vVal a) (vVal b) (vVal c)
                          , vNil = vNil a TS.:||:
                                      TS.ITE (vVal a) (vNil b) (vNil c)
                          }

    _ -> error ("XXX: " ++ show op)

  where
  op1 f a = Val { vVal = f (vVal a)
                , vNil = vNil a
                }

  op2 f a b = Val { vVal = f (vVal a) (vVal b)
                  , vNil = vNil a TS.:||: vNil b
                  }

  mkAtMostOne = case as of
                  [] -> valLit (Bool True)
                  _  -> Val { vVal = atMostOneVal (map vVal as)
                            , vNil = foldr1 (TS.:||:) (map vNil as)
                            }

  norVal xs = case xs of
                [] -> TS.Bool True
                _  -> TS.Not (foldr1 (TS.:||:) xs)

  atMostOneVal vs =
    case vs of
      []     -> TS.Bool True
      [_]    -> TS.Bool True
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
  case Map.lookup niName st of
    Just (TS.VBool b) ->
      if b then pure L.VNil
           else case Map.lookup vaName st of
                  Just v -> pure $ case v of
                                     TS.VInt x  -> L.VInt x
                                     TS.VBool x -> L.VBool x
                                     TS.VReal x -> L.VReal x
                  Nothing -> missing vaName
    Just v  -> bad niName "boolean" v
    Nothing -> missing niName
  where
  vaName = valName i
  niName = nilName i

  missing x = importError [ "[bug] Missing assignment"
                          , "*** Variable: " ++ show x
                          ]

  bad x msg v = importError [ "[bug] Unexpected value."
                            , "*** Variable: " ++ show x
                            , "*** Expected: " ++ msg
                            , "*** Value: " ++ show v
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





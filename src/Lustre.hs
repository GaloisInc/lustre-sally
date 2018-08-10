{-# Language OverloadedStrings, PatternSynonyms #-}
-- | Translate Core Lustre to a transtion system.
module Lustre (transNode, transAssert, importTrace) where

import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import qualified TransitionSystem as TS


import Language.Lustre.Core
import qualified Language.Lustre.Semantics.Value as L

transNode :: Node -> (TS.TransSystem, [TS.Expr])
transNode n = (ts, map transAssert (nAsserts n))
  where
  ts = TS.TransSystem
         { TS.tsVars    = Map.unions (map declareEqn (nEqns n))
         , TS.tsInputs  = Map.unions (map declareVar (nInputs n))
         , TS.tsInit    = initNode n
         , TS.tsTrans   = stepNode n
         }


-- | Assertions get translated into queries. In assertions, we treat
-- `nil` as `False`.   Assertion values deleted by a clock are considered
-- to be `True`.
transAssert :: Ident -> TS.Expr
transAssert i = (v delName TS.:>: zero) TS.:||:
               TS.Not (v nilName) TS.:&&: v valName
  where
  v f = TS.InCurState TS.::: f i

{- POSSIBLE ALTERNATIVE TRANSLATION

It seems that we might be able to translate things without the need
for the extra clock variable, by using the fact that clocks are quite
resatricted.  In particular, for every variable we know the variable that
corresponds to its clock.  Thus, we can represent each variable in the
same form as when viewed at the base rate (i.e., as if through a number
of calls to `current`)

Consider the following example:

                                CLOCK
    a: 1 2 3 4 5 6 7 8 9 10      base
    b: T F T F T F T F T F       base
    c: T T F T T F T F F T       base

    x = b when c                 c
    y = a when x                 x (which itself is on c)

    x: T F - F T - T - - F
    y: 1 _ _ _ 5 _ 7 _ _ _

in the "current" view the corresponding streams are:

    x': T F F F T T T T T F
    y': 1 1 1 1 5 5 7 7 7 7

Notise that we can't use `x'` directly when computing `y'`.
Instead we need consider all clocks involved, and we chnage
the output only if all of them are active.  Thus, we essentially
get the following equations:

    x' = current (b when c)
    y' = current (a when (x' && c))

So, with this translation, there is no need for `current` (i.e., it is
a no-op) because everything works at the base clock.
However, the filtering guards become a little more complex because we 
to consider the conjunction of clocks involved to determine when
to transition to a new value.


-}


{- NOTE:  Translating Variables
   ============================

Lustre variables can have a few special values:  in particular, they may
be `nil` or the may be "deleted", which happens when they are skipped by
a clock.  To modle these features, we translate each Lustre, say X, variable to
three variable in the model:

  X     : T
  X_nil : Bool
  X_del : Int

If `X_nil` is `true`, then this value is nil and the value of `X` is
irrelevant.

`X_del` is the depth of the clock that disabled this value.
It is never negative.
If it is 0, then the value is not disable and so it should be output.
We need an `Int` rather than just ` Bool`, because the `current` construct
restores only the values disable by the most recent clock (i.e., where
`X_del == 1`)

To summarize:
  * If X_del > 0, then this value is suppressed by a clock, and the other
                  variables are irrelevant.
  * if `X_del == 0 && X_nil`, then this value is `nil`
  * if `X_del == 0 && not X_nil` then the value is `X`
-}


data Val = Val { vVal :: TS.Expr
                 -- ^ type T.  The "normal" value.

               , vNil :: TS.Expr
                 -- ^ type Bool.
                 -- Indicates if the value is nil.
                 -- If so, the "normal" value is ignored.

               , vDel :: TS.Expr
                 -- ^ type Int (Nat, really, we need to assert that non neg.).
                 -- Indicates if this value is deleted by a clock.
                 -- 0 means that the value is present.
                 -- Larger numbers indicate that the value has been deleted
                 -- by a clock at the given nesting depth.
               }

zero, one, true, false :: TS.Expr
zero = TS.Int 0
one  = TS.Int 1
true = TS.Bool True
false = TS.Bool False

valLit :: Literal -> Val
valLit lit = Val { vVal = case lit of
                            Int n  -> TS.Int n
                            Bool b -> TS.Bool b
                            Real r -> TS.Real r
                 , vDel = zero
                 , vNil = false
                 }

valName :: Ident -> TS.Name
valName (Ident x) = TS.Name x

delName :: Ident -> TS.Name
delName (Ident x) = TS.Name (Text.append x "_del")

nilName :: Ident -> TS.Name
nilName (Ident x) = TS.Name (Text.append x "_nil")

valAtom :: TS.VarNameSpace -> Atom -> Val
valAtom ns atom =
  case atom of
    Lit l -> valLit l
    Var a -> Val { vVal = ns TS.::: valName a
                 , vDel = ns TS.::: delName a
                 , vNil = ns TS.::: nilName a
                 }


transType :: Type -> TS.Type
transType ty =
  case ty of
    TInt  -> TS.TInteger
    TBool -> TS.TBool
    TReal -> TS.TReal

declareVar :: Binder -> Map TS.Name TS.Type
declareVar (x ::: t) =
  Map.fromList [ (valName x, transType t)
               , (delName x, TS.TInteger)
               , (nilName x, TS.TBool)
               ]

declareEqn :: Eqn -> Map TS.Name TS.Type
declareEqn (x := _) = declareVar x


-- | Initial state for a node.
initNode :: Node -> TS.Expr
initNode n = ands (map initInput (nInputs n) ++ map initEqn (nEqns n))

-- | And tigether multiple boolean expressions.
ands :: [TS.Expr] -> TS.Expr
ands as =
  case as of
    [] -> TS.Bool True
    _  -> foldr1 (TS.:&&:) as


-- | Constraints on inputs.
initInput :: Binder -> TS.Expr
initInput (x ::: _) = (TS.InCurState TS.::: delName x) TS.:>=: zero

-- | Set the variables associated with a source variable.
setVals :: TS.VarNameSpace -> Ident -> Val -> TS.Expr
setVals ns x v = var valName TS.:==: vVal v TS.:&&:
                 var delName TS.:==: vDel v TS.:&&:
                 var nilName TS.:==: vNil v
  where
  var f = ns TS.::: f x

-- | Initial values for variables
initEqn :: Eqn -> TS.Expr
initEqn (x ::: _ := expr) =
  case expr of
    Atom a  -> letVars (atom a)
    a :-> _ -> letVars (atom a)
    Pre a   -> letVars (atom a) { vNil = true }

    a `When` b -> letVars newVals TS.:&&: aDel TS.:==: bDel
      where
      newVals =
        Val { vVal = aVal
            , vNil = aNil
            , vDel = TS.ITE -- Not too sure what happens if `b` is nil.
                       (bDel TS.:>: zero)
                       (bDel TS.:+: one) -- already delete, increase depth
                       (TS.ITE (TS.Not bNil TS.:&&: bVal) zero one)
                        -- `nil` treated as `false` for the moment
            }

      Val { vVal = aVal, vDel = aDel, vNil = aNil } = atom a
      Val { vVal = bVal, vDel = bDel, vNil = bNil } = atom b

    Current a ->
      letVars
        Val { vVal = aVal
            , vNil = (aDel TS.:==: one)  TS.:||:  aNil
            , vDel = TS.ITE (aDel TS.:>: one) (aDel TS.:-: one) zero
            }
      where Val { vVal = aVal, vDel = aDel, vNil = aNil } = atom a

    Prim f as ->
      case map vDel vs of
        [] -> base
        v : more -> foldl (TS.:&&:) base (map (v TS.:==:) more)
      where
      vs = map atom as
      base = letVars (primFun f vs)

  where
  atom = valAtom TS.InCurState
  letVars = setVals TS.InCurState x



primFun :: Op -> [Val] -> Val
primFun op as =
  case (op,as) of
    (Add, [a,b]) -> op2 (TS.:+:) a b
    (Eq,  [a,b]) -> op2 (TS.:==:) a b
    _ -> error ("XXX: " ++ show op)

  where
  op2 f a b = Val { vVal = f (vVal a) (vVal b)
                  , vNil = vNil a TS.:||: vNil b
                  , vDel = vDel a    -- which should be the same as `vDel b`
                  }


stepNode :: Node -> TS.Expr
stepNode n = ands (map stepEqn (nEqns n))


stepEqn :: Eqn -> TS.Expr
stepEqn (x ::: _ := expr) =
  case expr of
    Atom a  -> letVars (atom a)
    _ :-> b -> letVars (atom b)
    Pre a   -> letVars (valAtom TS.InCurState a)

    a `When` b -> letVars newVals TS.:&&: aDel TS.:==: bDel
      where
      newVals =
        Val { vVal = aVal
            , vNil = aNil
            , vDel = TS.ITE -- Not too sure what happens if `b` is nil.
                       (bDel TS.:>: zero)
                       (bDel TS.:+: one)
                       (TS.ITE (TS.Not bNil TS.:&&: bVal) zero one)
            }

      Val { vVal = aVal, vDel = aDel, vNil = aNil } = atom a
      Val { vVal = bVal, vDel = bDel, vNil = bNil } = atom b

    Current a ->
      letVars
      Val { vVal = TS.ITE (aDel TS.:==: zero) aVal' aVal
          , vNil = TS.ITE (aDel TS.:==: zero) aNil' aNil
          , vDel = TS.ITE (aDel TS.:>: one)
                          (aDel TS.:-: one)
                          zero
          }
      where
      Val { vVal = aVal, vDel = aDel, vNil = aNil } = atom a
      Val { vVal = aVal', vDel = _, vNil = aNil' } = valAtom TS.InCurState a

    Prim f as ->
      case map vDel vs of
        [] -> base
        v : more -> foldl (TS.:&&:) base (map (v TS.:==:) more)
      where
      vs = map atom as
      base = letVars (primFun f vs)

  where
  atom    = valAtom TS.InNextState
  letVars = setVals TS.InNextState x


--------------------------------------------------------------------------------
-- Importing of Traces


type ImportError = String

-- | Fail to import something
importError :: [String] -> Either ImportError a
importError = Left . unlines

-- | Import a Lustre identifier from the given assignment computed by Sally.
-- See "Translating Variables" for details of what's going on here.
importVar :: TS.VarVals -> Ident -> Either ImportError L.Step
importVar st i =
  case Map.lookup deName st of
    Just (TS.VInt n) ->
      case compare n 0 of
        GT -> pure (L.Skip n)
        EQ ->
          case Map.lookup niName st of
            Just (TS.VBool b) ->
              if b then pure (L.Emit L.VNil)
                   else case Map.lookup vaName st of
                          Just v -> pure $ L.Emit
                                         $ case v of
                                             TS.VInt x  -> L.VInt x
                                             TS.VBool x -> L.VBool x
                                             TS.VReal x -> L.VReal x
                          Nothing -> missing vaName
            Just v -> bad niName "boolean" v
            Nothing -> missing niName
        LT -> bad deName "non-negative integer" (TS.VInt n)
    Just v  -> bad deName "integer" v
    Nothing -> missing deName


  where
  vaName = valName i
  deName = delName i
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
importVars :: Set Ident -> TS.VarVals -> Either ImportError (Map Ident L.Step)
importVars vars st =
  do let is = Set.toList vars
     steps <- mapM (importVar st) is
     pure (Map.fromList (zip is steps))

importState :: Node -> TS.VarVals -> Either ImportError (Map Ident L.Step)
importState n = importVars $ Set.fromList [ x | x ::: _ := _ <- nEqns n ]

importInputs :: Node -> TS.VarVals -> Either ImportError (Map Ident L.Step)
importInputs n = importVars $ Set.fromList [ x | x ::: _ <- nInputs n ]

type LTrace = TS.Trace (Map Ident L.Step) (Map Ident L.Step)

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





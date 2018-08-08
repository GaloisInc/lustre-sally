{-# Language OverloadedStrings, PatternSynonyms #-}
module Lustre (transNode) where

import qualified Data.Text as Text
import           Data.Map (Map)
import qualified Data.Map as Map

import qualified TransitionSystem as TS


import Language.Lustre.Core

transNode :: Node -> TS.TransSystem
transNode n = TS.TransSystem
  { TS.tsVars    = Map.unions (map declareEqn (nEqns n))
  , TS.tsInputs  = Map.unions (map declareVar (nInputs n))
  , TS.tsInit    = initNode n
  , TS.tsTrans   = stepNode n
  }


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
    [] ->  TS.Bool True
    _  -> foldr1 (TS.:&&:) as


-- | Constraints on inputs.
initInput :: Binder -> TS.Expr
initInput (x ::: _) =
  (TS.InCurState TS.::: delName x) TS.:>=: zero



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

    Call f [a,b]
      | f == Name "+" -> letVars (initFun2 (TS.:+:) va vb) TS.:&&:
                          vDel va TS.:==: vDel vb
        where va = atom a
              vb = atom b

    Call _ _ -> error "Unknown call"

  where
  atom = valAtom TS.InCurState
  letVars = setVals TS.InCurState x



initFun2 :: (TS.Expr -> TS.Expr -> TS.Expr) -> Val -> Val -> Val
initFun2 f a b = Val { vVal = f (vVal a) (vVal b)
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

    Call f [a,b]
      | f == Name "+" -> letVars (initFun2 (TS.:+:) va vb) TS.:&&:
                         vDel va TS.:==: vDel vb
        where va = atom a
              vb = atom b

    Call _ _ -> error "UNsupported call"
  where
  atom    = valAtom TS.InNextState
  letVars = setVals TS.InNextState x


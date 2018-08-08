{-# Language PatternSynonyms #-}
module TransitionSystem where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(guard)

-- | A transition system.
data TransSystem = TransSystem
  { tsVars    :: Map Name Type    -- ^ Types of variables
  , tsInputs  :: Map Name Type    -- ^ Type of inputs
  , tsInit    :: Expr             -- ^ Over 'InCurState'
  , tsTrans   :: Expr             -- ^ Over any 'VarNameSpace'
  } deriving Show

-- | Abstraction of names.  (XXX: Add location)
newtype Name      = Name Text
                      deriving (Eq,Ord,Show)

-- | Variable namespaces.
data VarNameSpace = InCurState      -- ^ Variable refers to the current
                                    -- (or only) state.
                  | InNextState     -- ^ Variable refers to the next state.
                  | FromInput       -- ^ Variable refers to an input.
                      deriving (Eq,Ord,Show)

-- | The type of variables, annotated with a namespace.
data Var          = Var { varNS   :: !VarNameSpace    -- ^ Name space
                        , varName :: !Name            -- ^ Name
                        } deriving Show

-- | The type of expressions, used for predicates and terms.
data Expr         = EVar !Var
                  | EOp  !Op ![Expr]
                    deriving Show

-- | Types in the system.
data Type         = TInteger | TReal | TBool
                    deriving (Show,Eq)

-- | Constructors for expressions.
data Op           = OpEq | OpLt | OpLeq
                  | OpNot | OpAnd | OpOr
                  | OpAdd | OpSub | OpMul | OpDiv | OpDivInt | OpMod
                  | OpITE
                  | OpLit !Value
                    deriving Show

data Value        = VInt !Integer | VBool !Bool | VReal !Rational
                    deriving (Eq,Show)

infixr 2 :||:
infixr 3 :&&:
infix  4 :==:, :/=:, :<:, :<=:, :>:, :>=:
infixl 6 :+:, :-:
infixl 7 :*:, :/:
infix  9 :::

pattern ITE :: Expr -> Expr -> Expr -> Expr
pattern ITE x y z = EOp OpITE [x,y,z]

pattern (:==:) :: Expr -> Expr -> Expr
pattern x :==: y = EOp OpEq [x,y]

pattern (:<:) :: Expr -> Expr -> Expr
pattern x :<: y = EOp OpLt [x,y]

pattern (:<=:) :: Expr -> Expr -> Expr
pattern x :<=: y = EOp OpLeq [x,y]

pattern (:>:) :: Expr -> Expr -> Expr
pattern x :>: y = y :<: x

pattern (:>=:) :: Expr -> Expr -> Expr
pattern x :>=: y = y :<=: x

pattern Not :: Expr -> Expr
pattern Not x = EOp OpNot [x]

pattern (:/=:) :: Expr -> Expr -> Expr
pattern x :/=: y = Not (x :==: y)

pattern (:&&:) :: Expr -> Expr -> Expr
pattern x :&&: y = EOp OpAnd [x,y]

pattern (:||:) :: Expr -> Expr -> Expr
pattern x :||: y = EOp OpOr [x,y]

pattern (:+:) :: Expr -> Expr -> Expr
pattern x :+: y = EOp OpAdd [x,y]

pattern (:-:) :: Expr -> Expr -> Expr
pattern x :-: y = EOp OpSub [x,y]

pattern (:*:) :: Expr -> Expr -> Expr
pattern x :*: y = EOp OpMul [x,y]

pattern (:/:) :: Expr -> Expr -> Expr
pattern x :/: y = EOp OpDiv [x,y]

pattern Div :: Expr -> Expr -> Expr
pattern Div x y = EOp OpDivInt [x,y]

pattern Mod :: Expr -> Expr -> Expr
pattern Mod x y = EOp OpMod [x,y]

pattern Int :: Integer -> Expr
pattern Int x = EOp (OpLit (VInt x)) []

pattern Real :: Rational -> Expr
pattern Real x = EOp (OpLit (VReal x)) []

pattern Bool :: Bool -> Expr
pattern Bool x = EOp (OpLit (VBool x)) []

pattern (:::) :: VarNameSpace -> Name -> Expr
pattern x ::: y = EVar Var { varNS = x, varName = y }


--------------------------------------------------------------------------------
-- Validation

-- | Validate the well-formedness of a transition system.
validTS :: TransSystem -> Bool
validTS ts = validStatePred ts (tsInit ts) && validTransPred ts (tsTrans ts)

-- | Is this a valid state predicate.  These are suitable for the inital
-- state of a system, or for queries.
validStatePred :: TransSystem -> Expr -> Bool
validStatePred ts e =
  case typeOf ts (== InCurState) e of
    Just TBool -> True
    _          -> False

-- | Is this a valid transition predicate.  Used to describe the transitions
-- of the system.
validTransPred :: TransSystem -> Expr -> Bool
validTransPred ts e =
  case typeOf ts (const True) e of
    Just TBool -> True
    _          -> False


-- | Check he type of an expression in the given transition system.
typeOf ::
  TransSystem             {- ^ Context for expression -} ->
  (VarNameSpace -> Bool)  {- ^ Valid name spaces for variables -} ->
  Expr                    {- ^ Expression to check ^-} ->
  Maybe Type              {- ^ Type, if a well-formed expression -}

typeOf ts nsOk = check
  where
  check expr =
    case expr of

      EVar v ->
        do let ns = varNS v
           guard (nsOk ns)
           Map.lookup (varName v) $
             case ns of
               FromInput -> tsInputs ts
               _         -> tsVars ts

      EOp op pars ->
        case (op,pars) of
          (OpLit l, [])       -> return $! case l of
                                             VInt {}  -> TInteger
                                             VReal {} -> TReal
                                             VBool {} -> TBool


          (OpAdd, [x,y])      -> num2 x y
          (OpSub, [x,y])      -> num2 x y
          (OpMul, [x,y])      -> num2 x y
          (OpDiv, [x,y])      -> both TReal x y
          (OpDivInt , [x,y])  -> both TInteger x y
          (OpMod, [x,y])      -> both TInteger x y

          (OpNot,[x])         -> expect TBool x
          (OpAnd,[x,y])       -> both TBool x y
          (OpOr,[x,y])        -> both TBool x y

          (OpEq,[x,y])        -> rel x y
          (OpLt,[x,y])        -> rel x y
          (OpLeq,[x,y])       -> rel x y

          _                   -> Nothing

  expect t e =
    do t1 <- check e
       guard (t1 == t)
       return t

  both t e1 e2 = expect t e1 >> expect t e2 >> return t

  num2 a b =
    do t1 <- check a
       guard (t1 /= TBool)
       expect t1 b

  rel a b = do t1 <- check a
               t2 <- check b
               guard (t1 == t2)
               return TBool





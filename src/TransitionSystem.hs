{-# Language PatternSynonyms, OverloadedStrings #-}
module TransitionSystem where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(unless)
import Text.PrettyPrint as P (text,integer,double,Doc,(<>),(<+>),vcat)
import Data.Ratio(numerator,denominator)

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
                  | OpNot | OpAnd | OpOr | OpImplies
                  | OpNeg
                  | OpAdd | OpSub | OpMul | OpDiv | OpDivInt | OpMod
                  | OpITE
                  | OpToReal | OpToInt
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

pattern (:=>:) :: Expr -> Expr -> Expr
pattern x :=>: y = EOp OpImplies [x,y]

pattern (:+:) :: Expr -> Expr -> Expr
pattern x :+: y = EOp OpAdd [x,y]

pattern (:-:) :: Expr -> Expr -> Expr
pattern x :-: y = EOp OpSub [x,y]

pattern Neg :: Expr -> Expr
pattern Neg x = EOp OpNeg [x]

pattern (:*:) :: Expr -> Expr -> Expr
pattern x :*: y = EOp OpMul [x,y]

pattern (:/:) :: Expr -> Expr -> Expr
pattern x :/: y = EOp OpDiv [x,y]

pattern Div :: Expr -> Expr -> Expr
pattern Div x y = EOp OpDiv [x,y]

pattern Mod :: Expr -> Expr -> Expr
pattern Mod x y = EOp OpMod [x,y]

pattern ToReal :: Expr -> Expr
pattern ToReal x = EOp OpToReal [x]

pattern ToInt :: Expr -> Expr
pattern ToInt x = EOp OpToInt [x]

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

type Errs = [String]

-- | Validate the well-formedness of a transition system.
validTS :: TransSystem -> Errs
validTS ts = validStatePred ts (tsInit ts) ++
             validTransPred ts (tsTrans ts)

-- | Is this a valid state predicate.  These are suitable for the inital
-- state of a system, or for queries.
validStatePred :: TransSystem -> Expr -> Errs
validStatePred ts e =
  case typeOf ts (\x -> x == InCurState || x == FromInput) e of
    Right TBool -> []
    Right t     -> [ "Invalid type of state predicate, expected bool, got: "
                                                            ++ show t]
    Left err    -> [ "Bad state predicate", "*** Problem: " ++ err ]

-- | Is this a valid transition predicate.  Used to describe the transitions
-- of the system.
validTransPred :: TransSystem -> Expr -> Errs
validTransPred ts e =
  case typeOf ts (const True) e of
    Right TBool -> []
    Right t  -> [ "Invalid type of transitions predicate, expected bool, got: "
                                                            ++ show t]
    Left err    -> [ "Bad transiton predicate", "*** Problem: " ++ err ]


-- | Check he type of an expression in the given transition system.
typeOf ::
  TransSystem             {- ^ Context for expression -} ->
  (VarNameSpace -> Bool)  {- ^ Valid name spaces for variables -} ->
  Expr                    {- ^ Expression to check ^-} ->
  Either String Type      {- ^ Type, if a well-formed expression -}

typeOf ts nsOk = check
  where
  check expr =
    case expr of

      EVar v ->
        do let ns = varNS v
           unless (nsOk ns) (Left "Var not OK")
           let mp = case ns of
                      FromInput -> tsInputs ts
                      _         -> tsVars ts
           case Map.lookup (varName v) mp of
             Nothing -> Left $ unlines $ [ "Can't find var"
                                       , "*** Var: " ++ show v
                                       , "*** Map: " ++ show ns
                                       , "*** Content: "
                                       ] ++ map show (Map.toList mp)
             Just a  -> Right a

      EOp op pars ->
        let wrap x = case x of
                       Left err -> Left $ "When checking: " ++ show op ++
                                          "\n" ++ err
                       Right a -> Right a
        in wrap $
        case (op,pars) of
          (OpLit l, [])       -> return $! case l of
                                             VInt {}  -> TInteger
                                             VReal {} -> TReal
                                             VBool {} -> TBool


          (OpNeg, [x])        -> num1 x
          (OpAdd, [x,y])      -> num2 x y
          (OpSub, [x,y])      -> num2 x y
          (OpMul, [x,y])      -> num2 x y
          (OpDiv, [x,y])      -> both TReal x y
          (OpDivInt , [x,y])  -> both TInteger x y
          (OpMod, [x,y])      -> both TInteger x y

          (OpNot,[x])         -> expect TBool x
          (OpAnd,[x,y])       -> both TBool x y
          (OpOr,[x,y])        -> both TBool x y
          (OpImplies,[x,y])   -> both TBool x y

          (OpEq,[x,y])        -> rel x y
          (OpLt,[x,y])        -> rel x y
          (OpLeq,[x,y])       -> rel x y

          (OpITE,[x,y,z])     ->
             do _ <- expect TBool x
                t1 <- check y
                t2 <- check z
                unless (t1 == t2)
                  $ Left $ unlines [ "Arms of ITE have different types:"
                                   , "then: " ++ show t1
                                   , "else: " ++ show t2 ]
                Right t1

          (OpToReal, [x])     -> expect TInteger x >> pure TReal
          (OpToInt, [x])      -> expect TReal x >> pure TInteger

          _                   -> Left "BAD OP"


  expect t e =
    do t1 <- check e
       unless (t1 == t) $ Left $ unlines
                          [ "Expected: " ++ show t ++ ", got: " ++ show t1
                          , "Expr: " ++ show e ]
       return t

  both t e1 e2 = expect t e1 >> expect t e2 >> return t

  num1 a =
    do t1 <- check a
       unless (t1 /= TBool) (Left "Expected a numeric type")
       pure t1

  num2 a b =
    do t1 <- check a
       unless (t1 /= TBool) (Left "Expected a numeric type")
       expect t1 b

  rel a b = do t1 <- check a
               t2 <- check b
               unless (t1 == t2) (Left "Expected the same type")
               return TBool


--------------------------------------------------------------------------------
-- Traces, for processing counter-examples, etc.

-- | A generic trace with state 's' and transitions 'i', which might be empty.
data Trace s i = Trace { traceStart :: !s, traceSteps :: ![(i,s)] }
             deriving (Eq,Show)

-- | A mapping from names to values.
type VarVals = Map Name Value

-- | A trace in a transition system.
type TSTrace = Trace VarVals{-state-} VarVals{-inputs-}

--------------------------------------------------------------------------------

ppName :: Name -> Doc
ppName (Name x) = text (Text.unpack x)

ppValue :: Value -> Doc
ppValue val =
  case val of
    VBool b     -> text (show b)
    VInt i      -> integer i
    VReal n
      | toRational n1 == n -> double n1
      | otherwise -> integer (numerator n) P.<> "/" P.<> integer (denominator n)
      where n1 = fromRational n

ppVarVals :: VarVals -> Doc
ppVarVals = vcat . map ppDef . Map.toList
  where ppDef (x,y) = ppName x <+> "=" <+> ppValue y


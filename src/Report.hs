module Report where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Maybe(catMaybes)

import qualified Language.Lustre.AST  as P
import qualified Language.Lustre.Core as C
import Language.Lustre.Transform.NoStatic(CallSiteId)
import Language.Lustre.Transform.NoStruct(StructData(..))
import Language.Lustre.Transform.Desugar (ModelInfo(..), ModelFunInfo(..))
import qualified Language.Lustre.Semantics.Core as L
import qualified Language.Lustre.Semantics.Value as V
import Language.Lustre.Panic(panic)

import TransitionSystem(Trace(..))
import Lustre(LTrace)



type S = Map C.Ident L.Value

--------------------------------------------------------------------------------

data Loc = Loc { lModel     :: ModelInfo
               , lFunInfo   :: ModelFunInfo
               , lSubst     :: Map P.Ident P.Ident
               , lVars      :: Vars P.Ident
               }

atTop :: ModelInfo -> Maybe Loc
atTop mi =
  do let top = infoTop mi
     fi <- Map.lookup top (infoNodes mi)
     nd <- Map.lookup top (infoSource mi)
     pure Loc { lModel = mi
              , lFunInfo = fi
              , lSubst = Map.empty
              , lVars = nodeVars nd
              }

enterCall :: Loc -> CallSiteId -> Maybe Loc
enterCall l cs =
  do let mf = lFunInfo l
     xs       <- Map.lookup cs (mfiCallSites mf)
     (fnm,su) <- Map.lookup xs (mfiInlined mf)
     f <- case fnm of
            P.Unqual i -> pure i
            P.Qual {} -> panic "enterCall" ["Unsupported qualified name."]
     let mi = lModel l
     fi <- Map.lookup f (infoNodes mi)
     nd <- Map.lookup f (infoSource mi)
     let vars = nodeVars nd
         su1  = fmap (\i -> Map.findWithDefault i i (lSubst l)) su
     pure l { lFunInfo = fi
            , lSubst = su1
            , lVars = vars
            }

locSubs :: Loc -> [CallSiteId]
locSubs = Map.keys . mfiCallSites . lFunInfo

callTree :: ModelInfo -> Maybe (Tree Loc)
callTree mi = from <$> atTop mi
  where
  from l = T.Node l $ catMaybes $ map (go l) $ locSubs l
  go l c = from <$> enterCall l c

--------------------------------------------------------------------------------

lookupVars :: Loc -> S -> Vars (P.Ident, Maybe V.Value)
lookupVars l s = fmap lkp (lVars l)
  where lkp i = (i, lookupVar l s i)


lookupVar :: Loc -> S -> P.Ident -> Maybe V.Value
lookupVar l s i0 =
  case Map.lookup i (mfiStructs (lFunInfo l)) of
    Just si ->
      do si1 <- traverse (lookupVar l s) si
         pure (restruct si1)
    Nothing ->
      do ci <- Map.lookup i (infoCore (lModel l))
         v1 <- Map.lookup ci s
         reval v1
  where
  i = Map.findWithDefault i0 i0 (lSubst l)


reval :: L.Value -> Maybe V.Value
reval val =
  case val of
    L.VInt n  -> Just (V.VInt n)
    L.VBool n -> Just (V.VBool n)
    L.VReal n -> Just (V.VReal n)
    L.VNil    -> Nothing


restruct :: StructData V.Value -> V.Value
restruct str =
  case str of
    SLeaf a -> a
    SArray xs -> V.VArray (map restruct xs)
    SStruct x vs -> V.VStruct x (fmap (fmap restruct) vs)
    STuple {} -> panic "restruct" ["Unexpected tuple"]



--------------------------------------------------------------------------------
-- This is what we report
data Vars i = Vars
  { vIns  :: [i]
  , vLocs :: [i]
  , vOuts :: [i]
  }

instance Functor Vars where
  fmap f vs = Vars { vIns   = fmap f (vIns vs)
                   , vLocs  = fmap f (vLocs vs)
                   , vOuts  = fmap f (vOuts vs)
                   }

instance Foldable Vars where
  foldMap f vs = mconcat [ foldMap f (vIns vs)
                         , foldMap f (vLocs vs)
                         , foldMap f (vOuts vs)
                         ]

instance Traversable Vars where
  traverse f vs = Vars <$> traverse f (vIns vs)
                       <*> traverse f (vLocs vs)
                       <*> traverse f (vOuts vs)


nodeVars :: P.NodeDecl -> Vars P.Ident
nodeVars nd = Vars { vIns = fromB (P.nodeInputs prof)
                   , vLocs = fromB locs
                   , vOuts = fromB (P.nodeOutputs prof)
                   }
  where
  prof = P.nodeProfile nd
  locs = case P.nodeDef nd of
           Nothing -> []
           Just d -> [ b | P.LocalVar b <- P.nodeLocals d ]
  fromB = map P.binderDefines


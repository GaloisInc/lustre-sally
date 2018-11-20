{-# Language OverloadedStrings #-}
module Report where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP

import Language.Lustre.AST
import Language.Lustre.ModelState
import Language.Lustre.Pretty


import Lustre(LTrace)
import TransitionSystem(Trace(..))
import LSPanic(panic)


declareSource :: String -> String
declareSource s = show ("var source =" PP.$$ PP.nest 2 js)
  where JS js = text s

declareTrace :: ModelInfo -> LTrace -> String
declareTrace mi tr = show ("var trace =" PP.$$ PP.nest 2 js)
  where JS js = case computeCallTree mi of
                  Just ct -> renderTrace ct tr
                  Nothing -> panic "declareTrace"
                                [ "Failed to construct call tree." ]



--------------------------------------------------------------------------------

-- | All varaibles we need to show, organized by call site.
data CallTree = Fun Loc [(CallSiteId,CallTree)]

-- | Compute the call tree.
computeCallTree :: ModelInfo -> Maybe CallTree
computeCallTree mi = callTreeLoc <$> locTop mi

-- | Compute the call tree starting at a particular location.
-- Note that we ignore invalid call sites (which shouldn't be there?)
callTreeLoc :: Loc -> CallTree
callTreeLoc l = Fun l [ (c,callTreeLoc l1)
                      | c <- locCalls l
                      , Just l1 <- [ enterCall l c ] ]



--------------------------------------------------------------------------------


renderTrace :: CallTree -> LTrace -> JSON
renderTrace ct tr = renderJSMap (foldr step Map.empty (zip [0..] states))
  where
  step (i,stp) = renderCallTree i Nothing ct stp
  states       = traceStart tr : map snd (traceSteps tr)

renderCallTree :: Int -> Maybe CallSiteId -> CallTree -> S -> JsMap -> JsMap
renderCallTree stp mbcid (Fun l cs) s mp = foldr next start cs
  where
  start =
    case mbcid of
      Just cid -> renderCS stp cid l s mp
      Nothing  -> renderLoc (stp,"cs_top") l s mp

  next (c,t) = renderCallTree stp (Just c) t s


renderCS :: Int -> CallSiteId -> Loc -> S -> JsMap -> JsMap
renderCS step cid l s = renderLoc (step,nm) l s . addJS cid this
  where
  this = obj [ "cid"    ~> text nm
             , "source" ~> obj [ "from" ~> int (sourceLine (sourceFrom src))
                               , "to"   ~> int (sourceLine (sourceTo src))
                               , "step" ~> int step
                               ]
             ]
  nm   = callSiteName cid
  src  = range l




-- | Render the variables for this calls site.
renderLoc :: (Int,String) {- ^ Step and call site identifier -} ->
             Loc          {- ^ Funciton call to render -} ->
             S            {- ^ Current state -} ->
             JsMap -> JsMap
renderLoc cid l s = renderVars cid (lookupVars l s)

renderVars :: (Int,String) -> Vars (SourceIdent, Maybe SourceValue) ->
              JsMap -> JsMap
renderVars cid vs mp = foldr (renderVar cid) mp vs

renderVar :: (Int,String) -> (SourceIdent, Maybe SourceValue) -> JsMap -> JsMap
renderVar (s,cid) (i, mbV) =
  addJS i $ obj [ "value" ~> renderMaybeValue mbV , "cid"   ~> text cid
                , "step"  ~> int s
                ]

renderValue :: SourceValue -> JSON
renderValue v = text (showPP v)

renderMaybeValue :: Maybe SourceValue -> JSON
renderMaybeValue mb =
  case mb of
    Nothing -> text "?"
    Just v  -> renderValue v

-- | Maps: line -> (from-col,to-col) -> [json (e.g., value)]
type JsMap = Map Int (Map (Int,Int) [JSON])

addJS :: HasRange i => i -> JSON -> JsMap -> JsMap
addJS i js = Map.insertWith (Map.unionWith (++)) (sourceLine from)
           $ Map.singleton (sourceColumn from, sourceColumn to) [js]
  where
  loc  = range i
  from = sourceFrom loc
  to   = sourceTo loc

renderJSMap :: JsMap -> JSON
renderJSMap = obj . map renderLn . Map.toList
  where
  renderLn (l,xs)   = show l ~> renderAttrs xs
  renderAttrs       = array . map renderAttr . Map.toList
  renderAttr ((f,t),y)  = obj [ "from" ~> int f
                              , "to" ~> int t
                              , "attr" ~> array y
                              ]


--------------------------------------------------------------------------------

newtype JSON = JS { unJS :: PP.Doc }

text :: String -> JSON
text = JS . PP.text . show

int :: Int -> JSON
int = JS . PP.int

array :: [JSON] -> JSON
array = JS . block "[" "," "]" . map unJS

obj :: [(String,JSON)] -> JSON
obj = JS . block "{" "," "}" . map entry
  where entry (a,b) = unJS (text a) PP.<> ":" PP.<+> unJS b

(~>) :: String -> JSON -> (String,JSON)
a ~> b = (a,b)

block :: PP.Doc -> PP.Doc -> PP.Doc -> [PP.Doc] -> PP.Doc
block open sep close xs =
  case xs of
    [] -> open PP.<> close
    a : as -> PP.vcat $ (open PP.<+> a)
                      : [ sep PP.<+> b | b <- as ] ++
                        [ close ]






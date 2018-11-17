module Report where

import Data.Foldable(toList)

import Language.Lustre.AST
import Language.Lustre.ModelState
import Language.Lustre.Pretty

import Lustre(LTrace)
import TransitionSystem(Trace(..))
import LSPanic(panic)


declareSource :: String -> String
declareSource s = "var source = " ++ js
  where JS js = text s

declareTrace :: ModelInfo -> LTrace -> String
declareTrace mi tr = "var trace = " ++ js
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
renderTrace ct tr = array (map step states)
  where
  step   = array . renderCallTree Nothing ct
  states = traceStart tr : map snd (traceSteps tr)

renderCallTree :: Maybe CallSiteId -> CallTree -> S -> [JSON]
renderCallTree mbcid (Fun l cs) s = start ++ concatMap next cs
  where
  start =
    case mbcid of
      Just cid -> renderCS cid l s
      Nothing  -> renderLoc "cs_top" l s

  next (c,t) = renderCallTree (Just c) t s


renderCS :: CallSiteId -> Loc -> S -> [JSON]
renderCS cid l s = this : renderLoc nm l s
  where
  this = obj [ "line"   ~> int (sourceLine from)
             , "from"   ~> int (sourceColumn from)
             , "to"     ~> int (sourceColumn to)
             , "cid"    ~> text nm
             , "source" ~> obj [ "from" ~> int (sourceLine (sourceFrom src))
                               , "to"   ~> int (sourceLine (sourceTo src))
                               ]
             ]
  nm   = callSiteName cid

  loc  = range cid
  from = sourceFrom loc
  to   = sourceTo loc

  src  = range l




-- | Render the variables for this calls site.
renderLoc :: String {- ^ Call site identifier -} ->
             Loc    {- ^ Funciton call to render -} ->
             S      {- ^ Current state -} ->
             [JSON]
renderLoc cid l s = renderVars cid (lookupVars l s)

renderVars :: String -> Vars (SourceIdent, Maybe SourceValue) -> [JSON]
renderVars cid = map (renderVar cid) . toList

renderVar :: String -> (SourceIdent, Maybe SourceValue) -> JSON
renderVar cid (i, mbV) =
  obj [ "line"  ~> int (sourceLine from)
      , "from"  ~> int (sourceColumn from)
      , "to"    ~> int (sourceColumn to)
      , "value" ~> renderMaybeValue mbV
      , "cid"   ~> text cid
      ]
  where
  loc  = range i
  from = sourceFrom loc
  to   = sourceTo loc


renderValue :: SourceValue -> JSON
renderValue v = text (showPP v)

renderMaybeValue :: Maybe SourceValue -> JSON
renderMaybeValue mb =
  case mb of
    Nothing -> text "?"
    Just v  -> renderValue v


--------------------------------------------------------------------------------

newtype JSON = JS { unJS :: String }

text :: String -> JSON
text = JS . show

int :: Int -> JSON
int = JS . show

array :: [JSON] -> JSON
array = JS . block "[" "," "]" . map unJS

obj :: [(String,JSON)] -> JSON
obj = JS . block "{" "," "}" . map entry
  where entry (a,b) = unJS (text a) ++ ": " ++ unJS b

(~>) :: String -> JSON -> (String,JSON)
a ~> b = (a,b)

block :: String -> String -> String -> [String] -> String
block open sep close xs =
  case xs of
    [] -> open ++ close
    a : as -> unlines $ (open ++ " " ++ a)
                      : [ sep ++ " " ++ b | b <- as ] ++
                        [ close ]






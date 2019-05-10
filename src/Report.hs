{-# Language OverloadedStrings, BangPatterns #-}
module Report where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PP
import qualified Data.Text as Text
import Data.List(intercalate,transpose)

import Language.Lustre.AST
import Language.Lustre.Name(Label(..),OrigName)
import Language.Lustre.ModelState
import Language.Lustre.Pretty


import Lustre(LTrace)
import TransitionSystem(Trace(..))
import LSPanic(panic)

-- | Print a shorter, text based trace.  We just display the inputs
-- and outputs for the top node.
simpleTrace :: ModelInfo -> Label -> LTrace -> String
simpleTrace mi pn tr =
  Text.unpack (labText pn) ++ ":\n" ++
  (tabulate $ header : zipWith showStep [ 1 :: Integer .. ] (traceSteps tr))
  where
  Just topLoc = locTop mi

  header = "Step" : map showPP (vIns vs) ++ ("|->" : map showPP (vOuts vs))
    where
    vs = locVars topLoc

  showStep n (_, s) = show n : map sh (vIns vs) ++ ("" : map sh (vOuts vs))
    where
    vs        = lookupVars topLoc s
    sh (_,mb) = case mb of
                  Nothing -> "?"
                  Just v  -> showPP v

tabulate :: [[String]] -> String
tabulate rows0 = unlines (h : sep : hs)
  where
  h : hs     = map shRow rows
  sep        = intercalate "+" (map line colWidths)
  line n     = replicate n '-'
  rows       = map (map sp) rows0
  sp x       = ' ' : x ++ " "
  colWidths  = map (maximum . map length) (transpose rows)
  padTo n xs = xs ++ replicate (n - length xs) ' '
  shRow xs   = intercalate "|" (zipWith padTo colWidths xs)




declareSource :: String -> String
declareSource s = show ("var source =" PP.$$ PP.nest 2 js)
  where JS js = text (untab s)

declareTrace :: ModelInfo -> Label -> LTrace -> String
declareTrace mi pn tr = show ("var trace =" PP.$$ PP.nest 2 js)
  where
  JS js = case computeCallTree mi of
            Just ct ->
              obj [ "name" ~> text (Text.unpack (labText pn))
                  , "line" ~> int (sourceLine (sourceFrom (labRange pn)))
                  , "trace" ~> renderTrace ct tr
                  ]
            Nothing ->
              panic "declareTrace" [ "Failed to construct call tree." ]

untab :: String -> String
untab = go 0
  where
  go _ [] = []
  go !n (c : cs) =
    case c of
      '\n' -> '\n' : go 0 cs
      '\t' -> let r = n `mod` 8
                  sp = 8 - r
              in replicate sp ' ' ++ go (n + sp) cs
      _    -> c : go (n+1) cs


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
renderTrace ct tr = renderJSMap (foldr step cs (zip [0..] states))
  where
  cs = renderCallSites ct Map.empty
  step (i,stp) = renderCallTree i Nothing ct stp
  states       = traceStart tr : map snd (traceSteps tr)

renderCallTree :: Int -> Maybe CallSiteId -> CallTree -> S -> JsMap -> JsMap
renderCallTree stp mbcid (Fun l cs) s mp = foldr next start cs
  where
  cids = case mbcid of
           Nothing -> "cs_top"
           Just cid -> callSiteName cid
  start      = renderLoc (stp, cids) l s mp
  next (c,t) = renderCallTree stp (Just c) t s


renderCallSites :: CallTree -> JsMap -> JsMap
renderCallSites (Fun _ cs) mp = foldr renderCallSitesAt mp cs

renderCallSitesAt :: (CallSiteId, CallTree) -> JsMap -> JsMap
renderCallSitesAt (cid, t@(Fun l _)) mp =
  renderCallSites t (addJS cid this mp)
  where
  this = obj [ "cid"    ~> text nm
             , "source" ~> obj [ "from" ~> int (sourceLine (sourceFrom src))
                               , "to"   ~> int (sourceLine (sourceTo src))
                               ]
             ]
  src  = range l
  nm   = callSiteName cid



-- | Render the variables for this calls site.
renderLoc :: (Int,String) {- ^ Step and call site identifier -} ->
             Loc          {- ^ Funciton call to render -} ->
             S            {- ^ Current state -} ->
             JsMap -> JsMap
renderLoc cid l s = renderVars cid (lookupVars l s)

renderVars :: (Int,String) -> Vars (OrigName, Maybe SourceValue) ->
              JsMap -> JsMap
renderVars cid vs mp = foldr (renderVar cid) mp vs

renderVar :: (Int,String) -> (OrigName, Maybe SourceValue) -> JsMap -> JsMap
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






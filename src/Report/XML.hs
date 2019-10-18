module Report.XML where

import qualified Data.Text as Text
import Data.List(transpose)
import Text.XML.Light

import Language.Lustre.AST
import Language.Lustre.Name(OrigName,Label(..))
import Language.Lustre.ModelState
import Language.Lustre.Semantics.Value
import Language.Lustre.Pretty

import TransitionSystem(Trace(..))
import Lustre(LTrace)
import Report.CallTree


--------------------------------------------------------------------------------

labelLineCol :: Label -> (Int, Int)
labelLineCol l = (sourceLine startPos, sourceColumn startPos)
  where startPos = sourceFrom (range l)

(|->) :: String -> String -> Attr
(|->) key val = Attr (unqual key) val

propElem :: String -> Int -> Int -> [Element] -> Element
propElem name line col body =
  unode "Property" ( [ "name"    |-> name
                     , "line"    |-> show line
                     , "column"  |-> show col
                     ]
                   , body
                   )

runtimeElem :: String -> Double -> Element
runtimeElem timeout time =
    unode "Runtime" ( [ "unit"    |-> "sec"
                      , "timeout" |-> timeout
                      ]
                    , show time
                    )

kElem :: Int -> Element
kElem k = unode "K" (show k)

answerElem :: String -> String -> Element
answerElem src status =
  unode "Answer" ("source" |-> src, status)

cexElem :: [Element] -> Element
cexElem = unode "CounterExample"

topNodeElem :: String -> [Element] -> Element
topNodeElem name streams =
  unode "Node" (["name" |-> name], streams)

callNodeElem :: String -> String -> String -> [Element] -> Element
callNodeElem name ln col streams =
  unode "Node" ( [ "name"   |-> name
                 , "line"   |-> ln
                 , "column" |-> col
                 ]
               , streams
               )


-- TODO: can have different attributes
streamElem :: String -> String -> String -> [Element] -> Element
streamElem name typ cls vals =
  unode "Stream" ( [ "name"  |-> name
                   , "type"  |-> typ
                   , "class" |-> cls
                   ],
                   vals
                 )

valueElem :: Int -> String -> Element
valueElem instant value =
  unode "Value" ("instant" |-> show instant, value)

xmlValid :: Label -> Element
xmlValid pn =
  propElem (Text.unpack (labText pn)) l c
    [ runtimeElem "false" 0.0 -- TODO: real value
    , answerElem "kind" "valid" -- TODO: not always "kind"
    ]
  where (l, c) = labelLineCol pn

xmlUnknown :: String -> Label -> Element
xmlUnknown isTimeout pn =
  propElem (Text.unpack (labText pn)) l c
    [ runtimeElem isTimeout 0.0 -- TODO: real value
    , answerElem "kind" "unknown" -- TODO: not always "kind"
    ]
  where (l, c) = labelLineCol pn

xmlTrace ::  ModelInfo -> Label -> LTrace -> Element
xmlTrace mi pn tr =
  propElem (Text.unpack (labText pn)) l c $
    [ runtimeElem "false" 0.0
    , answerElem "kind" "falsifiable" -- TODO: not always "kind"
    , cexElem (case computeCallTree mi of
                 Just yes -> [ callTreeToXML yes tr ]
                 Nothing  -> []
              )
    ]
  where (l, c) = labelLineCol pn


callTreeToXML :: CallTree -> LTrace -> Element
callTreeToXML t@(Fun l _) tr =
  topNodeElem (show (pp (locName l))) (nodeElems t tr)

nodeElems :: CallTree -> LTrace -> [Element]
nodeElems (Fun l cs) tr = locVarsXML l tr ++
  [ callNodeElem (show (pp (locName lf)))
                 (show (sourceLine pos))
                 (show (sourceColumn pos))
                 (nodeElems tree tr)
  | (cid,tree@(Fun lf _)) <- cs
  , let pos = sourceFrom (range cid)
  ]



locVarsXML :: Loc -> LTrace -> [Element]
locVarsXML loc tr = map (varValToXML "input")  (vIns vs) ++
                    map (varValToXML "output") (vOuts vs) ++
                    map (varValToXML "local")  (vLocs vs)
  where
  vs = varVals loc tr


varValToXML :: String -> (OrigName,Type,[Maybe Value]) -> Element
varValToXML cls (x,t,vs) =
  streamElem (show (pp x)) (show (pp t)) cls
      [ valueElem n (show (pp v)) | (n,Just v) <- zip [ 1 .. ] vs ]

varVals :: Loc -> LTrace -> Vars (OrigName, Type, [Maybe Value])
varVals l tr = fmap extract
             $ rearrange (locVars l)
             $ map (lookupVars l . snd)
             $ traceSteps tr
  where
  extract ((x,t),vs) = (x,t,[v | (_,_,v) <- vs ])

-- assumes that all vars have the same shape
rearrange :: Vars i -> [Vars vs] -> Vars (i,[vs])
rearrange sh vs = Vars { vIns  = mk vIns vIns
                       , vLocs = mk vLocs vLocs
                       , vOuts = mk vOuts vOuts
                       }
  where
  mk f g = f sh `zip` transpose (map g vs)




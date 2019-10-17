module Report.XML where

import qualified Data.Text as Text
import Text.XML.Light



import Language.Lustre.AST
import Language.Lustre.Name(Label(..))
import Language.Lustre.ModelState




import Lustre(LTrace)


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

nodeElem :: String -> [Element] -> Element
nodeElem name streams =
  unode "Node" (["name" |-> name], streams)

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
xmlTrace _ pn _ =
  propElem (Text.unpack (labText pn)) l c
    [ runtimeElem "false" 0.0
    , answerElem "kind" "falsifiable" -- TODO: not always "kind"
    ] -- TODO: include trace
  where (l, c) = labelLineCol pn





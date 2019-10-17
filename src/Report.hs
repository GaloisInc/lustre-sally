{-# Language OverloadedStrings, BangPatterns #-}
module Report where

import qualified Data.Text as Text
import Data.List(intercalate,transpose)

import Language.Lustre.Name(Label(..))
import Language.Lustre.ModelState
import Language.Lustre.Pretty


import Lustre(LTrace)
import TransitionSystem(Trace(..))

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
    vs = fst <$> locVars topLoc

  showStep n (_, s) = show n : map sh (vIns vs) ++ ("" : map sh (vOuts vs))
    where
    vs        = lookupVars topLoc s
    sh (_,_,mb) = case mb of
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





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
simpleTrace :: Bool -> ModelInfo -> Label -> LTrace -> String
simpleTrace zeroBased mi pn tr =
  Text.unpack (labText pn) ++ ":\n" ++
  (tabulate $ header : zipWith showStep [ firstStep .. ]
                                        (isLast (traceSteps tr)))
  where
  Just topLoc = locTop mi

  header = "Step" : map showPP (vOuts vs) ++
          ("->"   : map showPP (vIns vs))
    where
    vs = fst <$> locVars topLoc

  firstStep :: Integer
  firstStep = if zeroBased then 0 else 1

  showStep n ((_, s),atEnd) =
    show n  : map sh (vOuts vs) ++
    (""     : map (if atEnd then const "" else sh) (vIns vs))
    where
    vs          = lookupVars topLoc s
    sh (_,_,mb) = case mb of
                    Nothing -> "?"
                    Just v  -> showPP v

  isLast = fst . foldr annot ([],True)
    where annot x (xs,a) = ((x,a):xs,False)

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

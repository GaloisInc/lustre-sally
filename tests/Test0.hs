{-# Language OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import Data.Text(Text)
import System.Process
import System.Directory
import System.IO
import System.Exit
import Control.Exception(finally)
import Text.PrettyPrint.ANSI.Leijen (vcat)

import TransitionSystem
import Sally

main :: IO ()
main = runTest (sys1, [q1])

runTest :: (TransSystem,  [Expr]) -> IO ()
runTest (ts,qs) =
  do let inp = foldr showsSExpr ""
             $ translateTS ts ++ map (translateQuery ts) qs
     let opts = [ "--engine=pdkind", "--show-trace" ]
     res <- sally "sally" opts inp
     case readSallyResult ts res of
       Right r  -> print r
       Left err -> do putStrLn $ "Failed to parse result: " ++ err
                      putStrLn res
                      exitFailure


sys1 :: TransSystem
sys1 = TransSystem
  { tsVars    = Map.fromList [ (x, TInteger) ]
  , tsInputs  = Map.fromList [ (i, TReal) ]
  , tsInit    = InCurState ::: x :==: Int 0
  , tsTrans   = InNextState ::: x :==: InCurState ::: x :+: Int 1
  }
  where
  x = Name "x"
  i = Name "i"

q1 :: Expr
q1 = x :<: Int 10
  where
  x = InCurState ::: Name "x"


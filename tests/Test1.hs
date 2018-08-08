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

import qualified TransitionSystem as TS
import Sally
import Lustre
import Language.Lustre.Core

main :: IO ()
main = runTest (sys1, [q2,q1])

runTest :: (TS.TransSystem,  [TS.Expr]) -> IO ()
runTest (ts,qs) =
  do let inp = foldr (\e es -> ppSExpr e $ showChar '\n' es) "\n"
             $ translateTS ts ++ map (translateQuery ts) qs
     putStrLn "=== Sally Input: =============="
     putStrLn inp
     putStrLn "==============================="
     let opts = [ "--engine=pdkind", "--show-trace" ]
     res <- sally "sally" opts inp
     case readSallyResults ts res of
       Right r  -> mapM_ print r
       Left err -> do putStrLn $ "Failed to parse result: " ++ err
                      putStrLn res
                      exitFailure


sys1 :: TS.TransSystem
sys1 = transNode
  Node { nName = Name "Test1"
       , nInputs = []
       , nOutputs = [ Ident "x"]
       , nAsserts = []
       , nEqns =
           [ Ident "x" ::: TInt := Atom (Lit (Int 1))
           ]
       }

q1 :: TS.Expr
q1 = x TS.:==: TS.Int 1
  where
  x = TS.InCurState TS.::: TS.Name "x"

q2 :: TS.Expr
q2 = x TS.:==: TS.Int 2
  where
  x = TS.InCurState TS.::: TS.Name "x"






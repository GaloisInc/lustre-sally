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
import Text.Show.Pretty(pPrint)

import qualified TransitionSystem as TS
import Sally
import Lustre
import Language.Lustre.Core

main :: IO ()
main = runTest sys1

runTest :: Node -> IO ()
runTest nd =
  do let (ts,qs) = transNode nd
         inp = foldr (\e es -> ppSExpr e $ showChar '\n' es) "\n"
             $ translateTS ts ++ map (translateQuery ts) qs
     putStrLn "=== Sally Input: =============="
     putStrLn inp
     putStrLn "==============================="
     let opts = [ "--engine=bmc", "--show-trace", "--output-language=mcmt" ]
     res <- sally "sally" opts inp
     case readSallyResults ts res of
       Right r  ->
          case traverse (traverse (importTrace nd)) r of
            Left err -> bad ("Failed to import trace: " ++ err) res
            Right as -> mapM_ pPrint as
       Left err -> bad ("Failed to parse result: " ++ err) res
  where
  bad err res =
    do putStrLn err
       putStrLn res
       exitFailure




sys1 :: Node
sys1 =
  Node { nInputs = []
       , nOutputs = [ Ident "x"]
       , nAsserts = [ Ident "a1", Ident "a2" ]
       , nEqns =
           [ Ident "y" ::: TInt := Pre (Var (Ident "x"))
           , Ident "p" ::: TInt := Prim Add [ Lit (Int 1), Var (Ident "y") ]
           , Ident "x" ::: TInt := Lit (Int 1) :-> Var (Ident "p")
           , Ident "a1" ::: TBool := Prim Eq
                                    [ Var (Ident "x"), Lit (Int 1) ]

           , Ident "a2" ::: TBool := Prim Eq
                                    [ Var (Ident "x"), Lit (Int 2) ]
           ]
       }







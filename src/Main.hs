{-# Language OverloadedStrings #-}
module Main(main) where

import System.Exit(exitFailure)
import Control.Monad(when,unless,zipWithM_)
import Control.Exception(catch, SomeException(..), displayException)
import Data.Char(isSpace)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map
import SimpleGetOpt
import Text.PrettyPrint as P ((<>), (<+>),nest,colon,integer,($$),vcat)
import System.IO(hFlush,stdout)
import Data.IORef(newIORef,writeIORef,readIORef)

import Language.Lustre.Parser
import Language.Lustre.AST
import Language.Lustre.Core(Node)
import Language.Lustre.Pretty
import Language.Lustre.Transform.Desugar(desugarNode)
import Language.Lustre.TypeCheck(quickCheckDecls)
import TransitionSystem(TransSystem)
import Sally
import Log
import Lustre

data Settings = Settings
  { file      :: FilePath
  , node      :: Text
  , saveCore  :: [FilePath]
  , saveSally :: [FilePath]
  , sallyOpts :: [String]
  , optTC     :: Bool
  , bmcLimit  :: Int
  , kindLimit :: Int
  }

options :: OptSpec Settings
options = OptSpec
  { progDefaults = Settings { file = "", node = "", sallyOpts = []
                            , saveSally = [], saveCore = [], optTC = True
                            , bmcLimit = 10, kindLimit = 10
                            }
  , progOptions =

      [ Option ['n'] ["node"]
          "Translate this node."
          $ ReqArg "IDENT" $ \a s ->
              if node s == ""
                then Right s { node = Text.pack a }
                else Left "Multiple nodes.  For the moment we support only one."

      , Option ['f'] ["file"]
        "Lustre file."
        $ ReqArg "FILE" $ \a s ->
          if file s == ""
              then Right s { file = a }
              else Left "Multiple files.  For now we support just one Lust file"

      , Option ['s'] ["sally"]
        "The value of this flag is a flag to sally"
        $ ReqArg "FLAG" $ \a s -> Right s { sallyOpts = a : sallyOpts s }

      , Option [] ["save-core"]
        "Save Core lustre output in this file"
        $ ReqArg "FILE" $ \a s -> Right s { saveCore = a : saveSally s }

      , Option [] ["save-sally"]
        "Save Sally output in this file"
        $ ReqArg "FILE" $ \a s -> Right s { saveSally = a : saveSally s }

      , Option [] ["no-tc"]
        "Disable type-checker"
        $ NoArg $ \s -> Right s { optTC = False }
      ]

  , progParamDocs = []
  , progParams    = \a _ -> Left ("Unexpected parameter: " ++ show a)
  }

main :: IO ()
main =
  do settings <- getOpts options
     when (file settings == "") $
       reportUsageError options ["No Lustre file was speicifed."]

     a <- parseProgramFromFileLatin1 (file settings)
     case a of
       ProgramDecls ds ->
           do ds1 <- doTC settings ds
              mainWork settings ds1
       _ -> fail "XXX: We don't support modules/packages for the moment."

  `catch` \(SomeException e) ->
            do putStrLn ("[FAIL] " ++ displayException e)
               exitFailure

fakeIdent :: Text -> Ident
fakeIdent x = Ident { identText    = x
                    , identPragmas = []
                    , identRange   = fakeRange
                    }
  where
  fakePos   = SourcePos 0 0 0 ""
  fakeRange = SourceRange fakePos fakePos


doTC :: Settings -> [TopDecl] -> IO [TopDecl]
doTC settings ds
  | not (optTC settings) = pure ds
  | otherwise =
    case quickCheckDecls ds of
      Left err -> print err >> exitFailure
      Right _  -> pure ds

mainWork :: Settings -> [TopDecl] -> IO ()
mainWork settings ds =
  do let nm = if node settings == ""
                  then Nothing
                  else Just (Unqual (fakeIdent (node settings)))
         (info,nd) = desugarNode ds nm
     mapM_ (\f -> writeFile f (show (pp nd))) (saveCore settings)

     let (ts,qs)  = transNode nd   -- transition system and queries

         -- S-expressions for the system and queries.
         ts_sexp  = unlines $ map (\e -> ppSExpr e "") $ translateTS ts
         qs_sexps = [ (x,ppSExpr (translateQuery ts q) "") | (x,q) <- qs ]

     -- Put all queries in a single file, if saving.
     unless (null (saveSally settings)) $
        do let inp = unlines (ts_sexp : map snd qs_sexps)
           mapM_ (\f -> writeFile f inp) (saveSally settings)

     putStrLn "Validating properties:"
     mapM_ (checkQuery settings nd ts ts_sexp) qs_sexps


checkQuery :: Settings -> Node -> TransSystem ->
                String -> (Text,String) -> IO ()
checkQuery settings nd ts_ast ts (l,q) =
  do putStr ("Property " ++ Text.unpack l ++ "... ")
     hFlush stdout
     attempt "inductive depth" (sallyKind (kindLimit settings)) $
       attempt "concrete depth" (sallyBMC (bmcLimit settings)) $
       sayWarn "Unknown" ("Valid up to depth " ++ show (bmcLimit settings))
  where
  attempt lab x orElse =
    do (maxD,res) <- runSally lab x
       case res of
         Valid     -> sayOK "Ok" (lab ++ " " ++ show maxD)
         Invalid r -> do sayFail "Failed" ""
                         printTrace r
         Unknown   -> orElse

  runSally lab opts =
    do prog <- newProgress
       maxVal <- newIORef 0
       let callback _ n = do progSay prog (lab ++ " " ++ show n)
                             writeIORef maxVal n -- assumes monotonic increase
       mbres <- sallyInteract "sally" opts callback (ts ++ q)
       -- res <- sally "sally" opts (ts ++ q)
       progClear prog
       v <- readIORef maxVal

       res <- case mbres of
                Left err -> bad "Sally error:" err
                Right res -> pure res
       case readSallyResult ts_ast res of
         Right (r,xs)
           | all isSpace xs  ->
              case traverse (importTrace nd) r of
                Right a -> pure (v,a)
                Left err -> bad "Failed to import trace" err
           | otherwise -> bad "Leftover stuff after answer" xs
         Left err -> bad ("Failed to parse result: " ++ err) res


printTrace :: LTrace -> IO ()
printTrace t =
  do putStrLn "Initial state:"
     print (ppState (traceStart t))
     putStrLn "Inputs:"
     zipWithM_ printInputs [1..] (map fst (traceSteps t))

  where
  ppState = ppIns

  printInputs n m = print ( ("Step" <+> integer n P.<> colon)
                            $$ nest 2 (ppIns m))

  ppIns = vcat . map ppIn . Map.toList
  ppIn (x,y) = pp x <+> "=" <+> pp y


sallyRequiredOpts :: [String]
sallyRequiredOpts = [ "--show-trace"
                    , "--no-lets"
                    , "--output-language=mcmt"
                    , "--yices2-mcsat"
                    , "--verbosity=1"
                    ]

sallyKind :: Int -> [String]
sallyKind lim = "--engine=kind"
              : ("--kind-max=" ++ show lim)
              : sallyRequiredOpts

sallyBMC :: Int -> [String]
sallyBMC lim = "--engine=bmc"
             : ("--bmc-max=" ++ show lim)
             : sallyRequiredOpts


bad :: String -> String -> IO a
bad err res =
  do sayFail "Error" err
     putStrLn res
     exitFailure



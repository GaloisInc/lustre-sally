{-# Language OverloadedStrings #-}
module Main(main) where

import System.Exit(exitFailure)
import Control.Monad(when)
import Control.Exception(catches, Handler(..), throwIO
                        , SomeException(..), displayException)
import Data.Char(isSpace)
import Data.Text(Text)
import qualified Data.Text as Text
import SimpleGetOpt
import System.IO(hFlush,stdout)
import System.FilePath(takeFileName,dropFileName,dropExtension,(</>))
import System.Directory(createDirectoryIfMissing)
import Data.IORef(newIORef,writeIORef,readIORef)

import Language.Lustre.Parser
import Language.Lustre.AST
import Language.Lustre.Core(Node)
import Language.Lustre.Pretty
import Language.Lustre.Monad(runLustre,LustreConf(..))
import Language.Lustre.Driver(quickNodeToCore)
import Language.Lustre.ModelState(ModelInfo)
import TransitionSystem(TransSystem)
import Sally
import Log
import Lustre
import Report(declareSource, declareTrace)
import SaveUI(saveUI)

data Settings = Settings
  { file      :: FilePath
  , node      :: Maybe Text
  , saveCore  :: Bool
  , saveSally :: Bool
  , optTC     :: Bool
  , bmcLimit  :: Int
  , kindLimit :: Int
  , outDir    :: FilePath
  }

options :: OptSpec Settings
options = OptSpec
  { progDefaults = Settings { file = "", node = Nothing
                            , saveSally = False, saveCore = False, optTC = True
                            , bmcLimit = 10, kindLimit = 10
                            , outDir = "results"
                            }
  , progOptions =

      [ Option ['n'] ["node"]
        "Translate this node."
        $ ReqArg "IDENT" $ \a s ->
            case node s of
              Nothing -> Right s { node = Just (Text.pack a) }
              Just _  ->
                Left "Multiple nodes.  For the moment we support only one."

      , Option ['d'] ["out-dir"]
        "Save output in this directory."
        $ ReqArg "DIR" $ \a s -> Right s { outDir = a }

      , Option [] ["save-core"]
        "Save Core lustre output in this file"
        $ NoArg $ \s -> Right s { saveCore = True }

      , Option [] ["save-sally"]
        "Save Sally output in this file"
        $ NoArg $ \s -> Right s { saveSally = True }

      , Option [] ["no-tc"]
        "Disable type-checker"
        $ NoArg $ \s -> Right s { optTC = False }
      ]

  , progParamDocs = [("FILE", "Lustre file containing model (required).")]
  , progParams    = \a s -> case file s of
                             "" -> Right s { file = a }
                             _  -> Left "We only support a single file for now."
  }

main :: IO ()
main =
  do settings <- getOptsX options

     when (file settings == "") $
       throwIO (GetOptException ["No Lustre file was speicifed."])

     a <- parseProgramFromFileLatin1 (file settings)
     case a of
       ProgramDecls ds -> mainWork settings ds
       _ -> bad "We don't support modules/packages for the moment." ""

  `catches`
      [ Handler $ \(GetOptException errs) ->
          do mapM_ (sayFail "Error") errs
             putStrLn ""
             putStrLn (usageString options)
             exitFailure

      , Handler $ \(ParseError mb) ->
          case mb of
            Nothing -> bad "Parse error at the end of the file." ""
            Just p  -> bad ("Parse error at " ++ prettySourcePos p) ""

      , Handler $ \(SomeException e) -> bad (displayException e) ""
     ]


bad :: String -> String -> IO a
bad err res =
  do sayFail "Error" err
     putStrLn res
     exitFailure



mainWork :: Settings -> [TopDecl] -> IO ()
mainWork settings ds =
  do let luConf = LustreConf { lustreInitialNameSeed = Nothing
                             , lustreLogHandle = stdout
                             }
     (info,nd) <- runLustre luConf (quickNodeToCore (node settings) ds)

     -- Save JS version of source model
     do txt <- readFile (file settings)
        saveOutput (outSourceFile settings) (declareSource txt)

    -- Save Core version of model, if needed
     when (saveCore settings) $
       saveOutput (outCoreFile settings) (show (pp nd))

     -- say "Lustre" ("State has " ++ show (length (nEqns nd)) ++ " variables.")

     let (ts,qs)  = transNode nd   -- transition system and queries

         -- S-expressions for the system and queries.
         ts_sexp  = unlines $ map (\e -> ppSExpr e "") $ translateTS ts
         qs_sexps = [ (x,ppSExpr (translateQuery ts q) "") | (x,q) <- qs ]

     -- Put all queries in a single file, if saving.
     when (saveSally settings) $
       saveOutput (outSallyFile settings) (unlines (ts_sexp : map snd qs_sexps))

     say "Lustre" "Validating properties:"
     outs <- mapM (checkQuery settings info nd ts ts_sexp) qs_sexps
     let summary = foldr status Valid outs
     say_ "Lustre" "Model status: "
     case summary of
       Valid     -> sayOK   "Valid" ""
       Unknown   -> sayWarn "Unknown" ""
       Invalid _ -> sayFail "Invalid" ""


  where
  status a s = case a of
                 Invalid _ -> Invalid ()
                 Unknown   -> case s of
                                Invalid _ -> s
                                _         -> Unknown
                 Valid     -> s


checkQuery :: Settings -> ModelInfo -> Node -> TransSystem ->
                String -> (PropName,String) -> IO (SallyResult ())
checkQuery settings mi nd ts_ast ts (l',q) =
  do say_ "Lustre" ("Property " ++ l ++ "... ")
     hFlush stdout
     attempt "considering simultaneous states to depth" (sallyKind (kindLimit settings)) $
       attempt "counter-example search depth" (sallyBMC (bmcLimit settings)) $
       do sayWarn "Unknown" ("Valid up to depth " ++ show (bmcLimit settings))
          pure Unknown
  where
  l = Text.unpack (pName l')

  attempt lab x orElse =
    do (maxD,res) <- runSally lab x
       case res of
         Valid ->
            do sayOK "Valid" (lab ++ " " ++ show maxD)
               pure Valid
         Invalid r ->
            do let propDir = outPropDir settings l
               sayFail "Invalid" ("See " ++ (propDir </> "index.html"))
               saveUI propDir
               saveOutput (outTraceFile settings l) (declareTrace mi l' r)
               pure (Invalid ())

         Unknown   -> orElse

  runSally lab opts =
    do prog <- newProgress
       maxVal <- newIORef 0
       let callback _ n = do progSay prog (lab ++ " " ++ show n)
                             writeIORef maxVal n -- assumes monotonic increase
       mbres <- sallyInteract "sally" opts callback (ts ++ q)
       progClear prog
       v <- readIORef maxVal

       res <- case mbres of
                Left err  -> bad "Sally error:" err
                Right res -> pure res
       case readSallyResult ts_ast res of
         Right (r,xs)
           | all isSpace xs  ->
              case traverse (importTrace nd) r of
                Right a -> pure (v,a)
                Left err -> bad "Failed to import trace" err
           | otherwise -> bad "Leftover stuff after answer" xs
         Left err -> bad ("Failed to parse result: " ++ err) res


--------------------------------------------------------------------------------
-- Sally flags
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


--------------------------------------------------------------------------------
-- Output directory structure

outFileDir :: Settings -> FilePath
outFileDir settings = outDir settings </>
                      dropExtension (takeFileName (file settings))
-- XXX: clashes if inputs in different directories?

outPropDir :: Settings -> String -> FilePath
outPropDir settings prop = outFileDir settings </> prop
-- XXX: escape, maybe

outSallyFile :: Settings -> FilePath
outSallyFile settings = outFileDir settings </> "problem.sally"

outSourceFile :: Settings -> FilePath
outSourceFile settings = outFileDir settings </> "source.js"

outCoreFile :: Settings -> FilePath
outCoreFile settings = outFileDir settings </> "model.core-lus"

outTraceFile :: Settings -> String -> FilePath
outTraceFile settigs prop = outPropDir settigs prop </> "trace.js"

saveOutput :: FilePath -> String -> IO ()
saveOutput fi out =
  do let dir = dropFileName fi
     createDirectoryIfMissing True dir
     writeFile fi out



{-# Language OverloadedStrings #-}
module Main(main) where

import System.Exit(exitFailure)
import Control.Monad(when,unless,foldM)
import Control.Exception(catches, Handler(..), throwIO
                        , SomeException(..), displayException)
import Control.Concurrent
          (newEmptyMVar,takeMVar,putMVar, forkIO, threadDelay, killThread)
import Data.Char(isSpace)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import SimpleGetOpt
import System.IO(hFlush,stdout)
import System.FilePath(takeFileName,dropFileName,dropExtension,(</>))
import System.Directory(createDirectoryIfMissing)
import Data.IORef(newIORef,writeIORef,readIORef)
import Text.Read(readMaybe)

import qualified Config
import Config.GetOpt (configValueGetOpt)

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
import Report(declareSource, simpleTrace, declareTrace)
import SaveUI(saveUI)

data Settings = Settings
  { file      :: FilePath
  , node      :: Maybe Text
  , saveCore  :: Bool
  , saveSally :: Bool
  , optTC     :: Bool
  , bmcLimit  :: Int
  , kindLimit :: Int
  , useMCSat  :: Bool
  , outDir    :: FilePath
  , useConfig :: FilePath
  , testMode  :: Bool
  , timeout   :: Maybe Int
  }

options :: OptSpec Settings
options = OptSpec
  { progDefaults = defaults
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

      , Option [] ["no-mcsat"]
        "Do not use MCSAT based solver."
        $ NoArg $ \s -> Right s { useMCSat = False }

      , Option [] ["counter-example-limit"]
        ("How big of a counter example to look for" ++
         " (default: " ++ show (bmcLimit defaults) ++ ")")
        $ ReqArg "NUM" $ \a s -> case readMaybe a of
                                   Just n | n >= 0 -> Right s { bmcLimit = n }
                                   _ -> Left "Invalid counter example limit."

      , Option [] ["proof-depth"]
        ("Limit to number of previous states to consider" ++
         " (default: " ++ show (kindLimit defaults) ++ ")")
        $ ReqArg "NUM" $ \a s -> case readMaybe a of
                                   Just n | n >= 0 -> Right s { kindLimit = n }
                                   _ -> Left "Invalid proof depth."

      , Option [] ["timeout"]
        "Terminate `sally` invocations after this many seconds"
        $ ReqArg "SECONDS" $ \a s ->
            case readMaybe a of
              Just n | n >= 0 -> Right s { timeout = Just n }
              _ -> Left ("Invalid timeout: " ++ a)

      , Option ['c'] ["config"]
        "Load additional settings from the given file"
        $ ReqArg "FILE" $ \a s -> Right s { useConfig = a }

      , Option [] ["test-mode"]
        "Run in testing-mode (prints more things on stdout)"
        $ NoArg $ \s -> Right s { testMode = True }
      ]

  , progParamDocs = [("FILE", "Lustre file containing model (required).")]
  , progParams    = \a s -> case file s of
                             "" -> Right s { file = a }
                             _  -> Left "We only support a single file for now."
  }

  where
  defaults = Settings
    { file = ""
    , node = Nothing
    , saveSally = False
    , saveCore = False
    , optTC = True
    , bmcLimit = 10
    , kindLimit = 10
    , useMCSat = True
    , outDir = "results"
    , useConfig = ""
    , testMode = False
    , timeout = Nothing
    }


-- | Parse some settings from a file and add them to the given settings.
settingsFromFile :: FilePath -> Settings -> IO Settings
settingsFromFile f start =
  do say "Lustre" ("Loading settings from: " ++ show f)
     txt <- TextIO.readFile f
     case Config.parse txt of
       Left (Config.ParseError loc msg) ->
         throwIO $ GetOptException [ "At " ++ l ++ ": " ++ msg ]
            where l = show (Config.posLine loc) ++ ":" ++
                      show (Config.posColumn loc)
       Right v ->
         case configValueGetOpt (specToGetOpt options) v of
           (setters,[]) -> foldM appSetter start setters
           (_,errs) -> throwIO (GetOptException errs)
  where
  appSetter s val = case val s of
                      Left err -> throwIO (GetOptException [err])
                      Right s1 -> pure s1



main :: IO ()
main =
  do settings0 <- getOptsX options
     settings  <- case useConfig settings0 of
                    "" -> pure settings0
                    f  -> settingsFromFile f settings0

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
                             , lustreNoTC = not (optTC settings)
                             }
     (info,nd) <- runLustre luConf (quickNodeToCore (node settings) ds)

     -- Save JS version of source model
     unless (testMode settings) $
       do txt <- readFile (file settings)
          saveOutput (outSourceFile settings) (declareSource txt)

     -- Save Core version of model, if needed
     let prettyCore = show (pp nd)
     when (saveCore settings) $
       saveOutput (outCoreFile settings) prettyCore
     when (testMode settings) $
       saveOutputTesting "Core Lustre" prettyCore

     let (ts,qs)  = transNode nd   -- transition system and queries

         -- S-expressions for the system and queries.
         ts_sexp  = unlines $ map (\e -> ppSExpr e "") $ translateTS ts
         qs_sexps = [ (x,ppSExpr (translateQuery ts q) "") | (x,q) <- qs ]

     -- Put all queries in a single file, if saving.
     let sallyTxt = unlines (ts_sexp : map snd qs_sexps)
     when (saveSally settings) $
       saveOutput (outSallyFile settings) sallyTxt
     when (testMode settings) $
       saveOutputTesting "Sally Model" sallyTxt

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
     attempt "considering simultaneous states to depth" (sallyKind settings) $
       attempt "counter-example search depth" (sallyBMC settings) $
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

         Invalid r
          | testMode settings ->
            do sayFail "Invalid" ""
               let siTr = simpleTrace  mi l' r
               saveOutputTesting "Trace" siTr
               pure (Invalid ())

          | otherwise ->
            do let propDir = outPropDir settings l
               sayFail "Invalid" ("See " ++ (propDir </> "index.html"))
               saveUI propDir
               let jsTr = declareTrace mi l' r
               saveOutput (outTraceFile settings l) jsTr
               pure (Invalid ())

         Unknown   -> orElse

  runSally lab opts =
    do prog <- if testMode settings then newTestProgress else newProgress
       maxVal <- newIORef 0
       let callback _ n = do progSay prog (lab ++ " " ++ show n)
                             writeIORef maxVal n -- assumes monotonic increase
           doSally = sallyInteract "sally" opts callback (ts ++ q)
       mbres <- case timeout settings of
                  Nothing -> doSally
                  Just t  -> withTimeout t doSally

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


withTimeout :: Int -> IO (Either String b) -> IO (Either String b)
withTimeout t act =
  do v <- newEmptyMVar
     tidSally <- forkIO $ do r <- act
                             putMVar v (Just r)
     tidTimer <- forkIO $ do threadDelay (t * 1000 * 1000)
                             killThread tidSally
                             putMVar v Nothing
     res <- takeMVar v
     case res of
       Nothing -> pure (Left "Timeout")
       Just r  -> do killThread tidTimer
                     pure r




--------------------------------------------------------------------------------
-- Sally flags
sallyRequiredOpts :: Settings -> [String]
sallyRequiredOpts s = ps ++
  [ "--show-trace"
  , "--no-lets"
  , "--output-language=mcmt"
  , "--verbosity=1"
  ]
  where ps = if useMCSat s then ["--yices2-mcsat"] else []

sallyKind :: Settings -> [String]
sallyKind s = "--engine=kind"
            : ("--kind-max=" ++ show lim)
            : sallyRequiredOpts s
  where lim = kindLimit s + 1
                -- it would appear Sally does not consider the last value

sallyBMC :: Settings -> [String]
sallyBMC s = "--engine=bmc"
           : ("--bmc-max=" ++ show lim)
           : sallyRequiredOpts s
  where lim = bmcLimit s


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

saveOutputTesting :: String -> String -> IO ()
saveOutputTesting lab val =
  do putStrLn lab
     putStrLn (replicate (length lab) '=')
     putStrLn ""
     putStrLn val


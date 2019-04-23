{-# Language OverloadedStrings #-}
module Main(main) where

import System.Exit(exitFailure)
import Control.Monad(when,foldM,unless)
import Control.Exception(catches, catch, Handler(..), throwIO
                        , SomeException(..), displayException)
import Control.Concurrent
          (newEmptyMVar,takeMVar,putMVar, forkIO, threadDelay, killThread)
import Data.Char(isSpace)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
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
import Language.Lustre.Phase(noPhases)
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
  , bmcLimit  :: Int
  , bmcLowerLimit :: Int
  , kindLimit :: Int
  , useMCSat  :: Bool
  , outDir    :: FilePath
  , useConfig :: FilePath
  , testMode  :: Bool
  , noTrace   :: Bool
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

      , Option [] ["no-mcsat"]
        "Do not use MCSAT based solver."
        $ NoArg $ \s -> Right s { useMCSat = False }

      , Option [] ["counter-example-limit"]
        ("How big of a counter example to look for" ++
         " (default: " ++ show (bmcLimit defaults) ++ ")")
        $ ReqArg "NUM" $ \a s -> case readMaybe a of
                                   Just n | n >= 0 -> Right s { bmcLimit = n }
                                   _ -> Left "Invalid counter example limit."

      , Option [] ["counter-example-lower-limit"]
        ("Smallest counter example of interest" ++
         " (default: " ++ show (bmcLowerLimit defaults) ++ ")")
        $ ReqArg "NUM" $ \a s ->
            case readMaybe a of
              Just n | n >= 0 -> Right s { bmcLowerLimit = n }
              _ -> Left "Invalid counter example lower limit."



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

      , Option [] ["no-trace"]
        "Do not print traces when a proof failes."
        $ NoArg $ \s -> Right s { noTrace = True }
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
    , bmcLimit = 10
    , bmcLowerLimit = 0
    , kindLimit = 10
    , useMCSat = True
    , outDir = "results"
    , useConfig = ""
    , testMode = False
    , noTrace = False
    , timeout = Nothing
    }


-- | Parse some settings from a file and add them to the given settings.
settingsFromFile :: Logger -> FilePath -> Settings -> IO Settings
settingsFromFile lgr f start =
  do say lgr Nothing "Lustre" ("Loading settings from: " ++ show f)
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


getSettings :: Logger -> IO Settings
getSettings l =
  do settings0 <- getOptsX options
     settings  <- case useConfig settings0 of
                    "" -> pure settings0
                    f  -> settingsFromFile l f settings0

     when (file settings == "") $
       throwIO (GetOptException ["No Lustre file was speicifed."])
     pure settings

  `catch` \(GetOptException errs) ->
      do mapM_ (sayFail l "Error") errs
         lPutStrLn l Nothing ""
         lPutStrLn l Nothing (usageString options)
         exitFailure





main :: IO ()
main =
  do settings <- getSettings =<< newLogger
     l <- if testMode settings then newTestLogger else newLogger
     do a <- parseProgramFromFileLatin1 (file settings)
        case a of
          ProgramDecls ds -> mainWork l settings ds
          _ -> bad l "We don't support modules/packages for the moment." ""

      `catches`
        [ Handler $ \(ParseError mb) ->
          case mb of
            Nothing -> bad l "Parse error at the end of the file." ""
            Just p  -> bad l ("Parse error at " ++ prettySourcePos p) ""

        , Handler $ \(SomeException e) -> bad l (displayException e) ""
        ]


bad :: Logger -> String -> String -> IO a
bad l err res =
  do sayFail l "Error" err
     lPutStrLn l Nothing res
     exitFailure



mainWork :: Logger -> Settings -> [TopDecl] -> IO ()
mainWork l settings ds =
  do let luConf = LustreConf { lustreInitialNameSeed = Nothing
                             , lustreLogHandle = stdout
                             , lustreDumpAfter = noPhases
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

     let (ts,qs)  = transNode nd   -- transition system and queries

         -- S-expressions for the system and queries.
         ts_sexp  = unlines $ map (\e -> ppSExpr e "") $ translateTS ts
         qs_sexps = [ (x,ppSExpr (translateQuery ts q) "") | (x,q) <- qs ]

     -- Put all queries in a single file, if saving.
     let sallyTxt = unlines (ts_sexp : map snd qs_sexps)
     when (saveSally settings) $
       saveOutput (outSallyFile settings) sallyTxt
     when (testMode settings) $ saveOutputTesting "Sally Model" sallyTxt

     say l Nothing "Lustre" "Validating properties:"
     outs <- mapM (checkQuery l settings info nd ts ts_sexp) qs_sexps
     let count x mp = Map.insertWith (+) (cvt x) (1::Int) mp
         summary = foldr count Map.empty outs
         getCount x = Map.findWithDefault 0 x summary
         modelStatus = if getCount (Invalid ()) > 0 then Invalid () else
                       if getCount Unknown      > 0 then Unknown else
                                                         Valid

     say l Nothing "Lustre" "Summary:"
     tab l 2 >> sayOK   l "Valid"   (show (getCount Valid))
     tab l 2 >> sayWarn l "Unknown" (show (getCount Unknown))
     tab l 2 >> sayFail l "Invalid" (show (getCount (Invalid ())))
     say_ l Nothing "Lustre" "Model status: "
     case modelStatus of
       Valid     -> sayOK   l "Valid" ""
       Unknown   -> sayWarn l "Unknown" ""
       Invalid _ -> sayFail l "Invalid" ""


  where
  cvt ans = case ans of
              Invalid _ -> Invalid ()
              Unknown   -> Unknown
              Valid     -> Valid


data Lab = Lab
  { labProg   :: Int -> String
  , labValid  :: Int -> String
  }

checkQuery :: Logger -> Settings -> ModelInfo -> Node -> TransSystem ->
                String -> (Label,String) -> IO (SallyResult ())
checkQuery lgr settings mi nd ts_ast ts (l',q) =
  do say lgr Nothing "Lustre" ("Property " ++ l ++ "...")
     hFlush stdout
     attempt kindLab (sallyKind settings) $
       attempt bmcLab (sallyBMC settings) $
          pure Unknown
  where
  l = Text.unpack (labText l')

  kindLab =
    Lab { labProg = \n -> "considering " ++ suf n ++ " "
        , labValid = \n -> "using " ++ suf n
        }
    where suf n = show n ++ " past state" ++ (if n == 1 then "" else "s")

  bmcLab =
    Lab { labProg = \n -> "searching up to depth " ++ show n ++ " "
        , labValid = \n -> "after searching to depth " ++ show n
        }


  attempt lab x orElse =
    do (maxD,mbRes) <- runSally lab x
       case mbRes of
         Nothing -> do sayWarn lgr "Timeout" ""
                       orElse

         Just Valid ->
            do sayOK lgr "Valid" (labValid lab maxD)
               pure Valid

         Just (Invalid r)
          | testMode settings ->
            do sayFail lgr "Invalid" ""
               let siTr = simpleTrace  mi l' r
               unless (noTrace settings) $ sayFail lgr "Trace" ('\n' : siTr)
               pure (Invalid ())

          | otherwise ->
            do let propDir = outPropDir settings l
               sayFail lgr "Invalid" ("See " ++ (propDir </> "index.html"))
               saveUI propDir
               let jsTr = declareTrace mi l' r
               let siTr = simpleTrace  mi l' r
               saveOutput (outTraceFile settings l) jsTr
               unless (noTrace settings) $ sayFail lgr "Trace" ('\n' : siTr)
               pure (Invalid ())

         Just Unknown -> do sayWarn lgr "Unknown" ""
                            orElse

  runSally lab opts =
    do maxVal <- newIORef 0
       let callback _ n = do lPutProg lgr (labProg lab n)
                             writeIORef maxVal n -- assumes monotonic increase
           doSally = do lPutStr lgr Nothing "  "
                        say_ lgr Nothing "Sally" ""
                        lNewProg lgr
                        sallyInteract "sally" opts callback (ts ++ q)

       mbres <- case timeout settings of
                  Nothing -> Just <$> doSally
                  Just t  -> withTimeout t doSally

       v <- readIORef maxVal

       case mbres of
         Nothing -> pure (v, Nothing)
         Just (Left err) -> bad lgr "Sally error" err
         Just (Right res) ->
           case readSallyResult ts_ast res of
             Right (r,xs)
               | all isSpace xs  ->
                  case traverse (importTrace nd) r of
                    Right a -> pure (v,Just a)
                    Left err -> bad lgr "Failed to import trace:" err
               | otherwise -> bad lgr "Leftover stuff after answer" xs
             Left err -> bad lgr "Failed to parse result:" err



withTimeout :: Int -> IO b -> IO (Maybe b)
withTimeout t act =
  do v <- newEmptyMVar
     tidSally <- forkIO $ do r <- act
                             putMVar v (Just r)
     tidTimer <- forkIO $ do threadDelay (t * 1000 * 1000)
                             killThread tidSally
                             putMVar v Nothing
     res <- takeMVar v
     case res of
       Nothing -> pure Nothing
       Just r  -> do killThread tidTimer
                     pure (Just r)




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
           : ("--bmc-min=" ++ show low)
           : ("--bmc-max=" ++ show lim)
           : sallyRequiredOpts s
  where lim = bmcLimit s
        low = bmcLowerLimit s


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


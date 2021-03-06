{-# Language OverloadedStrings #-}
module Main(main) where

import System.Exit(exitFailure,exitSuccess)
import Control.Monad(when,foldM,unless)
import Control.Exception(catches, catch, Handler(..), throwIO
                        , SomeException(..), displayException, Exception(..))
import Control.Concurrent
          (newEmptyMVar,takeMVar,putMVar, forkIO, threadDelay, killThread)
import Data.Char(isSpace)
import Data.Text(Text)
import Data.List(intercalate,sort,groupBy)
import Data.Maybe(listToMaybe)
import Data.Function(on)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Map as Map
import SimpleGetOpt
import System.IO(hFlush,stdout)
import System.FilePath(takeFileName,dropFileName,takeExtension,dropExtension
                      ,(</>))
import System.Directory(createDirectoryIfMissing,doesFileExist,
                        getCurrentDirectory,getDirectoryContents)
import Data.IORef(newIORef,writeIORef,readIORef)
import Data.Version(showVersion)
import Text.Read(readMaybe)

import qualified Config
import Config.GetOpt (configValueGetOpt)

import Language.Lustre.Parser
import Language.Lustre.AST
import Language.Lustre.Name
import Language.Lustre.Core(Node)
import Language.Lustre.Pretty
import Language.Lustre.Monad(runLustre,LustreConf(..))
import Language.Lustre.Driver(quickNodeToCore, infoTop)
import Language.Lustre.ModelState(ModelInfo)
import Language.Lustre.Phase(noPhases)
import qualified Language.Lustre.Error as L
import TransitionSystem(TransSystem)
import Sally
import Log
import Lustre
import Report(simpleTrace)
import Report.XML (xmlTrace,xmlValid,xmlUnknown)
import Report.JSON (declareSource, declareTrace)
import SaveUI(saveUI)
import Paths_lustre_sally(version)

data Settings = Settings
  { files     :: [FilePath]
  , node      :: Maybe Text
  , saveCore  :: Bool
  , saveSally :: Bool
  , bmcLimit  :: Int
  , bmcLowerLimit :: Int
  , kindLimit :: Int
  , yicesMode :: String
  , inDir     :: FilePath
  , outDir    :: FilePath
  , useConfig :: FilePath
  , testMode  :: Bool
  , noTrace   :: Bool
  , timeout   :: Maybe Int
  , produceXml :: Bool
  , printVersion :: Bool
  -- whether counter-example steps should start at step 0 or 1
  , zeroBasedCex :: Bool
  , sallyExe  :: FilePath
  , tmpDir :: Maybe FilePath -- use this as a tmp directory, or system default id nothing
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

      , Option [] ["in-dir"]
        "Read inputs from this directory."
        $ ReqArg "DIR" $ \a s -> Right s { inDir = a }

      , Option [] ["save-core"]
        "Save Core lustre output in this file"
        $ NoArg $ \s -> Right s { saveCore = True }

      , Option [] ["save-sally"]
        "Save Sally output in this file"
        $ NoArg $ \s -> Right s { saveSally = True }

      , Option [] ["sally-exe"]
        "Use this executable for `sally`"
        $ ReqArg "EXE" $ \a s -> Right s { sallyExe = a }

      , Option [] ["tmp-dir"]
        "Use this directory for writing tempory files."
        $ ReqArg "DIR" $ \a s -> Right s { tmpDir = Just a }

      , Option [] ["yices-mode"]
        "Specify how to use Yices"
        $ let modes = [ "hybrid", "dpllt", "mcsat" ]
              ty = intercalate "|" modes
          in ReqArg ty $ \a s ->
              if a `elem` modes
                  then Right s { yicesMode = a }
                  else Left ("Invalid Yices mode. Choose one of " ++ show modes)

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

      , Option [] ["version"]
        "Print version and exit."
        $ NoArg $ \s -> Right s { printVersion = True }

      , Option [] ["xml"]
        "Produce XML output in the style of Kind 2. Implies --zero-based-cex."
        $ NoArg $ \s -> Right s { produceXml = True, zeroBasedCex = True }

      , Option [] ["zero-based-cex"]
        "Numbers the first step of a counter-example 0 rather than 1."
        $ NoArg $ \s -> Right s { zeroBasedCex = True }
      ]

  , progParamDocs = [("FILE", "Lustre files containing model (required, unless --in-dir).")]
  , progParams    = \a s -> Right s { files = a : files s }
  }

  where
  defaults = Settings
    { files = []
    , node = Nothing
    , saveSally = False
    , saveCore = False
    , bmcLimit = 10
    , bmcLowerLimit = 0
    , kindLimit = 10
    , yicesMode = "hybrid"
    , outDir = "results"
    , inDir = ""
    , useConfig = ""
    , testMode = False
    , noTrace = False
    , timeout = Nothing
    , produceXml = False
    , printVersion = False
    , zeroBasedCex = False
    , sallyExe = "sally"
    , tmpDir = Nothing
    }


-- | Parse some settings from a file and add them to the given settings.
settingsFromFile :: Logger -> FilePath -> Settings -> IO Settings
settingsFromFile lgr f start =
  do sayInfo lgr "Lustre" ("Loading settings from: " ++ show f)
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


-- | If the '--in-dir' flag is provided, then we load settings and find input files
-- in the given directory.
settingsFromInDir :: Logger -> Settings -> IO Settings
settingsFromInDir l s
  | null (inDir s) = pure s
  | otherwise =
    do let dir = inDir s
           settingsFile = dir </> "settings"
       haveExtraSettings <- doesFileExist settingsFile
       s1 <- if haveExtraSettings then settingsFromFile l settingsFile s
                                  else pure s
       fs <- getDirectoryContents dir
       let lusFiles = [ dir </> f | f <- fs, takeExtension f == ".lus" ]
       pure s1 { files = lusFiles ++ files s1 }


getSettings :: Logger -> IO Settings
getSettings l =
  do settings0 <- getOptsX options
     when (printVersion settings0) $
       do putStrLn ("lustre-sally " ++ showVersion version)
          exitSuccess
     settings1 <- case useConfig settings0 of
                    "" -> pure settings0
                    f  -> settingsFromFile l f settings0
     settings2 <- settingsFromInDir l settings1
     when (null (files settings2)) $
       throwIO (GetOptException ["No Lustre file was specified."])
     pure settings2

  `catch` \(GetOptException errs) ->
      do mapM_ (sayFail l "Error") errs
         lPutStrLn l LogInfo ""
         lPutStrLn l LogInfo (usageString options)
         exitFailure





main :: IO ()
main =
  do settings <- getSettings =<< newLogger
     l <- if testMode settings then newTestLogger
          else if produceXml settings then newXmlLogger
          else newLogger
     mainWorkAllFiles l settings


data LSError = LSError String String deriving Show

instance Exception LSError

bad :: String -> String -> IO a
bad err res = throwIO (LSError err res)

sayBad :: Logger -> String -> String -> IO ()
sayBad l err res =
  do sayFail l "Error" err
     lPutStrLn l LogInfo res


mainWorkAllFiles :: Logger -> Settings -> IO ()
mainWorkAllFiles l settings =
  case files settings of
     [] -> pure ()
     f : fs ->
       do sayInfo l "Lustre" ("Loading model from: " ++ show f)
          oneFile f `catches` handlers
          mainWorkAllFiles l settings { files = fs }
  where
  oneFile f =
    do a <- parseProgramFromFileLatin1 f
       case a of
         ProgramDecls ds -> mainWork l settings ds
         _ -> bad "We don't support modules/packages for the moment." ""

  handlers =
    [ Handler $ \(ParseError p) -> sayBad l "Parse error" =<< showContexts [p]

    , Handler $ \(LSError a b) -> sayBad l a b

    , Handler $ \lerr -> prettyLustreError l lerr

    , Handler $ \(SomeException e) -> sayBad l (displayException e) ""
    ]






mainWork :: Logger -> Settings -> [TopDecl] -> IO ()
mainWork l settings ds =
  do let luConf = LustreConf { lustreInitialNameSeed = Nothing
                             , lustreLogHandle = stdout
                             , lustreDumpAfter = noPhases
                             }
     (info,nd) <- runLustre luConf (quickNodeToCore (node settings) ds)

     -- Save JS version of source model
     unless (testMode settings) $
       do txt <- readFile (head (files settings))
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

     when (produceXml settings) $ do
       let top = Text.unpack (origNameTextName (infoTop info))
       sayElement l (xmlStartElem top)
       lPutLn l

     sayInfo l "Lustre" "Validating properties:"
     outs <- mapM (checkQuery l settings info nd ts ts_sexp) qs_sexps
     let count x mp = Map.insertWith (+) (cvt x) (1::Int) mp
         summary = foldr count Map.empty outs
         getCount x = Map.findWithDefault 0 x summary
         modelStatus = if getCount (Invalid ()) > 0 then Invalid () else
                       if getCount Unknown      > 0 then Unknown else
                                                         Valid

     unless (produceXml settings) $ do
       sayInfo l "Lustre" "Summary:"
       tab l 2 >> sayOK   l "Valid"   (show (getCount Valid))
       tab l 2 >> sayWarn l "Unknown" (show (getCount Unknown))
       tab l 2 >> sayFail l "Invalid" (show (getCount (Invalid ())))
       lSay l LogInfo "Lustre" "Model status: "
       case modelStatus of
         Valid     -> sayOK   l "Valid" ""
         Unknown   -> sayWarn l "Unknown" ""
         Invalid _ -> sayFail l "Invalid" ""

     when (produceXml settings) $ do
       sayElement l xmlStopElem
       lPutLn l
       lPutStrRaw l resultsEndStr
       lPutLn l


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
  do sayInfo lgr "Lustre" ("Property " ++ l ++ "...")
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
         Nothing
          | produceXml settings ->
             do sayElement lgr (xmlUnknown "true" l' maxD)
                lPutLn lgr
                orElse

          | otherwise ->
             do sayWarn lgr "Timeout" ""
                orElse

         Just Valid
          | produceXml settings ->
            do sayElement lgr (xmlValid l' maxD)
               lPutLn lgr
               pure Valid

          | otherwise ->
            do sayOK lgr "Valid" (labValid lab maxD)
               pure Valid

         Just (Invalid r)
          | testMode settings ->
            do sayFail lgr "Invalid" ""
               let siTr = simpleTrace (zeroBasedCex settings) mi l' r
               unless (noTrace settings) $ sayFail lgr "Trace" ('\n' : siTr)
               pure (Invalid ())

          | produceXml settings ->
            do propURI <- outPropFileURI settings l
               sayElement lgr (xmlTrace propURI (zeroBasedCex settings) mi l' r maxD)
               lPutLn lgr
               saveUI (outPropDir settings l)
               saveOutput (outTraceFile settings l) (declareTrace mi l' r)
               pure (Invalid ())

          | otherwise ->
            do let propDir = outPropDir settings l
               propURI <- outPropFileURI settings l
               sayFail lgr "Invalid" ("See " ++ propURI)
               saveUI propDir
               let jsTr = declareTrace mi l' r
               let siTr = simpleTrace  (zeroBasedCex settings) mi l' r
               saveOutput (outTraceFile settings l) jsTr
               unless (noTrace settings) $
                 sayFail lgr "Trace" ('\n' : siTr)
               pure (Invalid ())

         Just Unknown
           | produceXml settings ->
             do sayElement lgr (xmlUnknown "false" l' maxD)
                lPutLn lgr
                orElse

           | otherwise ->
             do sayWarn lgr "Unknown" ""
                orElse

  runSally lab opts =
    do maxVal <- newIORef 0
       let callback _ n = do lPutProg lgr (labProg lab n)
                             writeIORef maxVal n -- assumes monotonic increase
           doSally = do lPutStr lgr LogInfo "  "
                        lSay lgr LogInfo "Sally" ""
                        lNewProg lgr
                        sallyInteract (tmpDir settings)
                                      (sallyExe settings)
                                      opts callback (ts ++ q)

       mbres <- case timeout settings of
                  Nothing -> Just <$> doSally
                  Just t  -> withTimeout t doSally

       v <- readIORef maxVal

       case mbres of
         Nothing -> pure (v, Nothing)
         Just (Left err) -> bad "Sally error" err
         Just (Right res) ->
           case readSallyResult ts_ast res of
             Right (r,xs)
               | all isSpace xs  ->
                  case traverse (importTrace nd) r of
                    Right a -> pure (v,Just a)
                    Left err -> bad "Failed to import trace:" err
               | otherwise -> bad "Leftover stuff after answer" xs
             Left err -> bad "Failed to parse result:" err



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
sallyRequiredOpts s =
  [ "--yices2-mode=" ++ yicesMode s
  , "--show-trace"
  , "--no-lets"
  , "--output-language=mcmt"
  , "--verbosity=1"
  ]

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
outFileDir settings =
  outDir settings </> dropExtension (takeFileName (head (files settings)))
  -- XXX: Name clashes if same name indifferent directoryies.

outPropDir :: Settings -> String -> FilePath
outPropDir settings prop = outFileDir settings </> prop
-- XXX: escape, maybe

outPropFileURI :: Settings -> String -> IO String
outPropFileURI settings prop =
  do
    cwd <- getCurrentDirectory
    pure $ "file://" <> cwd </> outPropDir settings prop </> "index.html"

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

-- XXX: Use logger!
saveOutputTesting :: String -> String -> IO ()
saveOutputTesting lab val =
  do putStrLn lab
     putStrLn (replicate (length lab) '=')
     putStrLn ""
     putStrLn val




--------------------------------------------------------------------------------
-- Show nice errors

prettyLustreError :: Logger -> L.LustreError -> IO ()
prettyLustreError l e =
  case e of
    L.ResolverError re ->
      case re of
        L.InvalidConstantExpression {} -> simple
        L.UndefinedName x -> prettyNameErr x ("Undefined name " <> back x) []
        L.AmbiguousName x a b ->
          prettyNameErr x ("Ambiguous name " <> back x)
            [ "It may refer to:"
            , "  • Constant defined at " <> prettySourcePos (locOf a) <> ", or"
            , "  • Variable defined at " <> prettySourcePos (locOf b)
            ]

        L.RepeatedDefinitions ~(x:xs) ->
          do cs <- showContexts (map range (x : xs))
             sayBad l ("Multiple definitions for " <> back x) cs

        L.BadRecursiveDefs {} -> simple

    L.TCError ls msg -> sayBad l (show msg) =<< showContexts ls
    L.BadEntryPoint {} -> simple

  where
  simple = sayBad l (show (pp e)) ""
  back x = "`" <> show (pp x) <> "`"
  locOf x = sourceFrom (range x)
  prettyNameErr x m ms =
    do let r = range x
       ctx <- showContexts [r]
       let msg = prettySourcePos (sourceFrom r) <> ": " <> m
       sayBad l msg (unlines (ms ++ [ctx]))


-- How many things to underline
errRangeLen :: SourceRange -> Int
errRangeLen r
  | sourceFile x == sourceFile y &&
    sourceLine x == sourceLine y = abs (sourceColumn x - sourceColumn y) + 1
  | otherwise = 1
    where x = sourceFrom r
          y = sourceTo r

showContexts :: [SourceRange] -> IO String
showContexts cs = ("\n"++) . concat <$> mapM showContextFile byLine
  where
  prep r = let p = sourceFrom r
           in (sourceFile p, (sourceLine p, (sourceColumn p, errRangeLen r)))

  sorted = sort (map prep cs)
  byFile = regroup sorted
  byLine = [ (Text.unpack f,regroup ls) | (f,ls) <- byFile ]

regroup :: Eq a => [(a,b)] -> [(a,[b])]
regroup = map cvt . groupBy ((==) `on` fst)
  where cvt xs = (fst (head xs), map snd xs)

showContextFile :: (FilePath,[(Int,[(Int,Int)])]) -> IO String
showContextFile (file,ls) =
  do contents <- lines <$> (readFile file
                              `catch` \SomeException{} -> pure "")
     pure $ unlines $ file : concatMap (showLine contents) ls

  where
  -- we could avoid duplicating lines, not sure if it's worth
  showLine :: [String] -> (Int,[(Int,Int)]) -> [String]
  showLine contents (lineNum,cs) = concatMap showPos cs
    where
    l = case listToMaybe (drop (lineNum-1) contents) of
          Nothing -> "(missing)"
          Just t  -> t

    lineNumTxt = show lineNum

    showPos :: (Int,Int) -> [String]
    showPos (col,len) =
      [ ""
      , " " <> lineNumTxt <> " | " <> l
      , replicate (3 + length lineNumTxt + col) ' ' <> replicate len '^'
      ]



-- from crucible-c/src/Log.hs
module Log where

import System.Console.ANSI
import System.IO(stdout,hFlush)
import Control.Monad(when)
import Control.Concurrent(newMVar,modifyMVar_)
import Text.XML.Light


data Logger = Logger
  { lSay :: LogType -> String -> String -> IO ()
    -- ^ Print a message (no new-line) with the given type and prefix.

  , lPutStr    :: LogType -> String -> IO ()
    -- ^ Print a message (no new-line) of the given type.

  , lPutStrRaw :: String -> IO ()
    -- ^ Print a message (no new-line) with no type interpretation.

  , lPutLn   :: IO ()
    -- ^ Print a newline, if appropriate.

  , lPutProg   :: String -> IO ()
    -- ^ Print a progress message.

  , lNewProg :: IO ()
    -- ^ Start a new batch of progress reporting.

  , lClearProg :: IO ()
    -- ^ Clear last progress message.
  }


data LogType
  = LogOK
  | LogFail
  | LogWarn
  | LogEM
  | LogInfo

logColor :: LogType -> Maybe Color
logColor LogOK = Just Green
logColor LogFail = Just Red
logColor LogWarn = Just Yellow
logColor LogEM = Just White
logColor LogInfo = Nothing

logName :: LogType -> String
logName LogOK = "success"
logName LogFail = "failure"
logName LogWarn = "warning"
logName LogEM = "important"
logName LogInfo = "info"

tab :: Logger -> Int -> IO ()
tab l n = lPutStr l LogInfo (replicate n ' ')

say :: Logger -> LogType -> String -> String -> IO ()
say l typ x y = lSay l typ x y >> lPutLn l

sayOK :: Logger -> String -> String -> IO ()
sayOK l = say l LogOK

sayFail :: Logger -> String -> String -> IO ()
sayFail l = say l LogFail

sayWarn :: Logger -> String -> String -> IO ()
sayWarn l = say l LogWarn

sayEM :: Logger -> String -> String -> IO ()
sayEM l = say l LogEM

sayInfo :: Logger -> String -> String -> IO ()
sayInfo l = say l LogInfo

lPutStrLn :: Logger -> LogType -> String -> IO ()
lPutStrLn l typ x = lPutStr l typ x >> lPutLn l


--------------------------------------------------------------------------------

newTestLogger :: IO Logger
newTestLogger = pure Logger
  { lSay        = \_ x y ->
      do putStr "["
         putStr x
         putStr ("]" ++ if null y then y else " "++ y)
  , lPutStr     = \_ x -> putStr x >> hFlush stdout
  , lPutStrRaw  = \x -> putStr x >> hFlush stdout
  , lPutLn      = putStrLn ""
  , lPutProg    = \_   -> pure ()
  , lNewProg    = pure ()
  , lClearProg  = pure ()
  }


newLogger :: IO Logger
newLogger =
  do r <- newMVar False
     pure Logger
       { lSay = \typ x y ->
           do putStr "["
              case logColor typ of
                Nothing -> putStr x
                Just c  -> printCol c x
              putStr ("]" ++ if null y then y else " "++ y)
       , lPutStr = \typ x ->
           do case logColor typ of
                Nothing -> putStr x
                Just c  -> printCol c x
              hFlush stdout

       , lPutStrRaw = \x -> putStr x >> hFlush stdout
       , lPutLn = putStrLn "" >> hFlush stdout

       , lPutProg = \msg ->
           modifyMVar_ r $ \inProg ->
             do if inProg then restoreCursor else saveCursor
                clearFromCursorToLineEnd
                putStr msg
                hFlush stdout
                pure True

      , lNewProg = modifyMVar_ r $ \_ -> pure False

      , lClearProg =
          modifyMVar_ r $ \inProg ->
          do when inProg $
                do restoreCursor
                   clearFromCursorToLineEnd
             pure False
      }


xmlStartElem :: String -> Element
xmlStartElem top =
  unode "AnalysisStart" [ Attr (unqual "top") top
                        , Attr (unqual "concrete") ""
                        , Attr (unqual "abstract") ""
                        , Attr (unqual "assumptions") ""
                        ]

xmlStopElem :: Element
xmlStopElem = unode "AnalysisStop" ()

xmlLogElem :: LogType -> String -> Element
xmlLogElem typ msg =
  unode "Log" ( [ Attr (unqual "class") (logName typ)
                , Attr (unqual "source") "parse"
                ]
              , msg
              )

resultsStartStr :: String
resultsStartStr = "<Results xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" enabled=\"bmc,kind,pdkind\" timeout=\"0.000000\" bmc_max=\"0\" compositional=\"false\" modular=\"false\">"

resultsEndStr :: String
resultsEndStr = "</Results>"

sayElement :: Logger -> Element -> IO ()
sayElement l e = lPutStrRaw l (ppElement e)

putXmlStr :: LogType -> String -> IO ()
putXmlStr typ x =
  do putStrLn (ppElement (xmlLogElem typ x))
     hFlush stdout

newXmlLogger :: IO Logger
newXmlLogger = do
  putStrLn xml_header
  putStrLn resultsStartStr
  pure Logger
    { lSay = \typ x y ->
        putXmlStr typ ("[" ++ x ++ "]" ++ if null y then y else " "++ y)
    , lPutStr = putXmlStr
    , lPutStrRaw = \x -> putStr x >> hFlush stdout
    , lPutLn = putStrLn "" >> hFlush stdout
    , lPutProg = putXmlStr LogInfo
    , lNewProg = return ()
    , lClearProg = return ()
    }

-- the cursor even if the input is 0.
curBack :: Int -> IO ()
curBack n = when (n > 0) (cursorBackward n)

printCol :: Color -> String -> IO ()
printCol c x =
  do setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
     putStr x
     setSGR [Reset]


-- from crucible-c/src/Log.hs
module Log where

import System.Console.ANSI
import System.IO(stdout,hFlush)
import Control.Monad(when)
import Control.Concurrent(newMVar,modifyMVar_)


data Logger = Logger
  { lPutStr    :: Maybe Color -> String -> IO ()
    -- ^ Print something (no new-line), in the given color if any.

  , lPutProg   :: String -> IO ()
    -- ^ Print a progress message.

  , lNewProg :: IO ()
    -- ^ Start a new batch of progress reporting.

  , lClearProg :: IO ()
    -- ^ Clear last progress message.
  }


say_ :: Logger -> Maybe Color -> String -> String -> IO ()
say_ l col x y =
  do lPutStr l Nothing "["
     lPutStr l col x
     lPutStr l Nothing ("] " ++ y)

say :: Logger -> Maybe Color -> String -> String -> IO ()
say l col x y = say_ l col x y >> lPutStr l Nothing "\n"

sayOK :: Logger -> String -> String -> IO ()
sayOK l = say l (Just Green)

sayFail :: Logger -> String -> String -> IO ()
sayFail l = say l (Just Red)

sayWarn :: Logger -> String -> String -> IO ()
sayWarn l = say l (Just Yellow)

sayEM :: Logger -> String -> String -> IO ()
sayEM l = say l (Just White)


lPutStrLn :: Logger -> Maybe Color -> String -> IO ()
lPutStrLn l c x = lPutStr l c (x ++ "\n")


--------------------------------------------------------------------------------

newTestLogger :: IO Logger
newTestLogger = pure Logger
  { lPutStr     = \_ x -> putStr x >> hFlush stdout
  , lPutProg    = \_   -> pure ()
  , lNewProg    = pure ()
  , lClearProg  = pure ()
  }


newLogger :: IO Logger
newLogger =
  do r <- newMVar False
     pure Logger
       { lPutStr = \mbC x ->
           do case mbC of
                Nothing -> putStr x
                Just c  -> printCol c x
              hFlush stdout

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

-- the cursor even if the input is 0.
curBack :: Int -> IO ()
curBack n = when (n > 0) (cursorBackward n)

printCol :: Color -> String -> IO ()
printCol c x =
  do setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
     putStr x
     setSGR [Reset]





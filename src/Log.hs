-- from crucible-c/src/Log.hs
module Log where

import System.Console.ANSI
import System.IO(stdout,hFlush)
import Control.Monad(when)
import Data.IORef(newIORef,readIORef,writeIORef)

sayOK :: String -> String -> IO ()
sayOK = sayCol Green

sayFail :: String -> String -> IO ()
sayFail = sayCol Red

sayWarn :: String -> String -> IO ()
sayWarn = sayCol Yellow

say :: String -> String -> IO ()
say x y = putStrLn ("[" ++ x ++ "] " ++ y)

say_ :: String -> String -> IO ()
say_ x y = putStr ("[" ++ x ++ "] " ++ y)

sayCol :: Color -> String -> String -> IO ()
sayCol col x y =
  do putStr "["
     printCol col x
     putStrLn ("] " ++ y)



printCol :: Color -> String -> IO ()
printCol c x =
  do setSGR [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid c]
     putStr x
     setSGR [Reset]

data Progress = Progress
  { progSay  :: String -> IO ()
  , progClear :: IO ()
  }

newProgress :: IO Progress
newProgress =
  do r <- newIORef 0
     return Progress
       { progSay = \msg -> do lastLen <- readIORef r
                              curBack lastLen
                              let newLen = length msg
                                  pad    = replicate (lastLen - newLen) ' '
                              putStr (msg ++ pad)
                              hFlush stdout
                              writeIORef r newLen
      , progClear = do lastLen <- readIORef r
                       curBack lastLen
                       putStr (replicate lastLen ' ')
                       hFlush stdout
                       curBack lastLen
      }

-- There appears to be a bug (or a feautre I misunderstand) which affects
-- the cursor even if the input is 0.
curBack :: Int -> IO ()
curBack n = when (n > 0) (cursorBackward n)



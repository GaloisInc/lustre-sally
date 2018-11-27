module Main(main) where

import System.FilePath(dropExtension,takeFileName,(</>),(<.>))
import System.Directory(createDirectoryIfMissing)
import Data.Char(toUpper)

main :: IO ()
main = mapM_ translate [ "ui/index.html"
                       , "ui/ui.js"
                       , "ui/style.css"
                       , "ui/jquery.js"
                       ]

translate :: FilePath -> IO ()
translate file =
  do putStrLn $ file ++ " -> " ++ newFile
     txt <- readFile file
     createDirectoryIfMissing True dir
     writeFile newFile (content txt)
  where
  m         = baseModName file
  dir       = "src" </> "UI"
  newFile   = dir </> m <.> "hs"
  content c =
    unlines [ "-- WARNING: Automatically generated from " ++ show file
            , "-- See executable `generate-ui`"
            , "{-# Language QuasiQuotes #-}"
            , "module UI." ++ m ++ " where"
            , ""
            , "import Text.RawString.QQ"
            , ""
            , "source :: String"
            , "source = " ++ show (takeFileName file)
            , ""
            , "content :: String"
            , "content = [r|"
            , c
            , "|]"
            ]

baseModName :: FilePath -> String
baseModName x =
  case dropExtension (takeFileName x) of
    a : as -> toUpper a : as
    []     -> error "Empty file name"


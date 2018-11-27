module SaveUI (saveUI) where

import System.Directory(createDirectoryIfMissing)
import System.FilePath

import qualified UI.Index
import qualified UI.Ui
import qualified UI.Style
import qualified UI.Jquery

saveUI :: FilePath -> IO ()
saveUI dir =
  do createDirectoryIfMissing True dir
     writeFile (dir </> UI.Index.source)  UI.Index.content
     writeFile (dir </> UI.Ui.source)     UI.Ui.content
     writeFile (dir </> UI.Style.source)  UI.Style.content
     writeFile (dir </> UI.Jquery.source) UI.Jquery.content


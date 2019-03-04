{-# Language TemplateHaskell #-}
module LSPanic (panic,Panic.Panic(..)) where

import qualified Panic
import Panic hiding (panic)

data LustreToSally = LustreToSally

panic :: String -> [String] -> a
panic = Panic.panic LustreToSally

instance PanicComponent LustreToSally where
  panicComponentName _ = "lustre-sally"
  panicComponentIssues _ = "https://github.com/GaloisInc/lustre-sally/issues"
  panicComponentRevision = $useGitRevision


{-# Language TemplateHaskell #-}
module LSPanic (panic,Panic.Panic(..)) where

import qualified Panic
import Panic hiding (panic)

data LustreToSally = LustreToSally

panic :: String -> [String] -> a
panic = Panic.panic LustreToSally

instance PanicComponent LustreToSally where
  panicComponentName _ = "Lustre to Sally translator."
  panicComponentIssues _ = "(XXX: no issue tracker yet)"
  panicComponentRevision = $useGitRevision


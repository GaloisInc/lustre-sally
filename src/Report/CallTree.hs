{-# Language OverloadedStrings, BangPatterns #-}
module Report.CallTree where

import Language.Lustre.ModelState

-- | All varaibles we need to show, organized by call site.
data CallTree = Fun Loc [(CallSiteId,CallTree)]

-- | Compute the call tree.
computeCallTree :: ModelInfo -> Maybe CallTree
computeCallTree mi = callTreeLoc <$> locTop mi

-- | Compute the call tree starting at a particular location.
-- Note that we ignore invalid call sites (which shouldn't be there?)
callTreeLoc :: Loc -> CallTree
callTreeLoc l = Fun l [ (c,callTreeLoc l1)
                      | c <- locCalls l
                      , Just l1 <- [ enterCall l c ] ]




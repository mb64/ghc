{-
(c) The University of Glasgow, 2000-2006
-}

{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

module FastFunctions (
    inlinePerformIO,
  ) where

#include "HsVersions.h"

import GHC.Exts
import GHC.IO   (IO(..))

-- Just like unsafePerformIO, but we inline it.
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #)   -> r

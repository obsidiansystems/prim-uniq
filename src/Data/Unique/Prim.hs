{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Unique.Prim
    ( Uniq, getUniq, RealWorld
    ) where

import Unsafe.Unique.Prim
import Control.Monad.Primitive (RealWorld)
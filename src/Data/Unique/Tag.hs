{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Unique.Tag
    ( Tag
    , newTag
    
    , RealWorld
    
    , (:=)(..)
    , GEq(..)
    
    , GOrdering(..)
    , GCompare(..)
    ) where

import Data.GADT.Compare
import Unsafe.Unique.Tag
import Control.Monad.Primitive (RealWorld)
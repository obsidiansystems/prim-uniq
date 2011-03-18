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
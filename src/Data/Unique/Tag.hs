module Data.Unique.Tag
    ( Tag
    , newTag
    , GOrdering(..)
    , GCompare(..)
    , geq
    ) where

import Data.GADT.Compare
import Unsafe.Unique.Tag
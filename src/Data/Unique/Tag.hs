module Data.Unique.Tag
    ( GOrdering(..)
    , GCompare(..)
    , geq
    , Tag
    , newTag
    ) where

import Data.GADT.Compare
import Unsafe.Unique.Tag
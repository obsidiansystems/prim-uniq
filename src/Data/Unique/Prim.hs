{-# LANGUAGE BangPatterns #-}
module Data.Unique.Prim (Uniq, getUniq, unsafeMkUniq) where

import Control.Monad.Primitive
import Data.IORef
import System.IO.Unsafe

-- A smaller numeric type could be used, such as Word or Word64, but I
-- want to be able to guarantee uniqueness.
-- Smaller types would require either checking for overflow or accepting
-- the possibility of aliasing.  The latter would make this type's usage
-- in 'Tag' unsound.

-- |A 'Uniq' is a value that can only be constructed under controlled 
-- conditions (in IO or ST, basically), and once constructed can only be
-- compared to 'Uniq' values created under the same conditions (in the same
-- monad).  Upon comparison, a 'Uniq' is ONLY ever equal to itself.  Beyond
-- that, no promises regarding ordering are made except that once constructed
-- the order is deterministic and a proper ordering relation (eg, > is 
-- transitive and irreflexive, etc.)
newtype Uniq s = Uniq Integer deriving (Eq, Ord)
instance Show (Uniq s) where showsPrec p (Uniq u) = showsPrec p u

{-# NOINLINE nextUniq #-}
nextUniq :: IORef Integer
nextUniq = unsafePerformIO (newIORef 0)

-- |Construct a new 'Uniq' that is equal to itself, unequal to every other
-- 'Uniq' constructed in the same monad, and incomparable to every 'Uniq' 
-- constructed in any other monad.
getUniq :: PrimMonad m => m (Uniq (PrimState m))
getUniq = unsafePrimToPrim (atomicModifyIORef nextUniq (\(!u) -> let u' = u+1 in u' `seq` (u', Uniq u)))

-- |For the implementation of 'Uniq' construction in new monads, this operation
-- is exposed.  Users must accept responsibility for ensuring true uniqueness 
-- across the lifetime of the resulting 'Uniq' value.  Failure to do so could
-- lead to type unsoundness in code depending on uniqueness as a type witness
-- (eg, TypeExperiments.GCompare.Tag).
unsafeMkUniq :: Integer -> Uniq s
unsafeMkUniq n = Uniq n
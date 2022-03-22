module Duce.Vector where

import Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable as Mut
import Duce.Prelude
import qualified Duce.Vector.Mutable as Mut

-- |
-- >>> fromReverseListN 3 [1,2,3] :: Data.Vector.Vector Int
-- [3,2,1]
{-# INLINE fromReverseListN #-}
fromReverseListN :: Vector v a => Int -> [a] -> v a
fromReverseListN size list =
  initialized size $ \mv -> Mut.writeListInReverseOrderStartingFrom mv (pred size) list

-- |
-- >>> fromReverseStrictListN 3 [1,2,3] :: Data.Vector.Vector Int
-- [3,2,1]
{-# INLINE fromReverseStrictListN #-}
fromReverseStrictListN :: Vector v a => Int -> List a -> v a
fromReverseStrictListN size list =
  initialized size $ \mv -> Mut.writeStrictListInReverseOrderStartingFrom mv (pred size) list

{-# INLINE initialized #-}
initialized :: Vector v a => Int -> (forall s. Mutable v s a -> ST s ()) -> v a
initialized size initialize = runST $ do
  mv <- Mut.unsafeNew size
  initialize mv
  freeze mv

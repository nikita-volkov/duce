module Duce.Vector.Mutable where

import Data.Vector.Generic.Mutable
import Duce.Prelude

{-# INLINE writeListInReverseOrderStartingFrom #-}
writeListInReverseOrderStartingFrom :: MVector v a => v s a -> Int -> [a] -> ST s ()
writeListInReverseOrderStartingFrom v =
  let loop !index = \case
        value : tail -> do
          unsafeWrite v index value
          loop (pred index) tail
        _ -> return ()
   in loop

{-# INLINE writeStrictListInReverseOrderStartingFrom #-}
writeStrictListInReverseOrderStartingFrom :: MVector v a => v s a -> Int -> List a -> ST s ()
writeStrictListInReverseOrderStartingFrom v =
  let loop !index = \case
        Cons value tail -> do
          unsafeWrite v index value
          loop (pred index) tail
        _ -> return ()
   in loop

module Duce.Util.Multimap
(
  Multimap,
  empty,
  size,
  insert,
  minView,
  minViewWithKey,
)
where

import Duce.Prelude hiding (insert, empty)
import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque


data Multimap a b = Multimap !Int !(Map.Map a (Deque b))

empty :: Multimap a b
empty = Multimap 0 Map.empty

size :: Multimap a b -> Int
size (Multimap x _) = x

insert :: Ord a => a -> b -> Multimap a b -> Multimap a b
insert a b (Multimap size map) = let
  map' = Map.alter
    (\ case
      Just deque -> Just (Deque.snoc b deque)
      Nothing -> Just (pure b))
    a map
  in Multimap (succ size) map'

minView :: Ord a => Multimap a b -> Maybe (b, Multimap a b)
minView = fmap (first snd) . minViewWithKey

minViewWithKey :: Ord a => Multimap a b -> Maybe ((a, b), Multimap a b)
minViewWithKey (Multimap size map) = case Map.minViewWithKey map of
  Just ((k, deque), map') -> case Deque.uncons deque of
    Just (head, tail) -> if Deque.null tail
      then Just ((k, head), Multimap (pred size) map')
      else let
        size' = pred size
        map'' = Map.insert k tail map'
        in Just ((k, head), Multimap size' map'')
    _ -> error "Empty bucket"
  Nothing -> Nothing

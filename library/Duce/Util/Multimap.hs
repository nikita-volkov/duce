module Duce.Util.Multimap
(
  Multimap,
  empty,
  insert,
  minView,
  minViewWithKey,
)
where

import Duce.Prelude hiding (insert, empty)
import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque


newtype Multimap a b = Multimap (Map.Map a (Deque b))

empty :: Multimap a b
empty = Multimap Map.empty

insert :: Ord a => a -> b -> Multimap a b -> Multimap a b
insert a b (Multimap map) = let
  map' = Map.alter
    (\ case
      Just deque -> Just (Deque.snoc b deque)
      Nothing -> Just (pure b))
    a map
  in Multimap map'

minView :: Ord a => Multimap a b -> Maybe (b, Multimap a b)
minView = fmap (first snd) . minViewWithKey

minViewWithKey :: Ord a => Multimap a b -> Maybe ((a, b), Multimap a b)
minViewWithKey (Multimap map) = case Map.minViewWithKey map of
  Just ((k, deque), map') -> case Deque.uncons deque of
    Just (head, tail) -> if Deque.null tail
      then Just ((k, head), Multimap map')
      else let
        map'' = Map.insert k tail map'
        in Just ((k, head), Multimap map'')
    _ -> error "Empty bucket"
  Nothing -> Nothing

module Duce.Plan where

import Duce.Prelude

-- *

data Plan i o r
  = TerminatePlan r
  | YieldPlan o (Plan i o r)
  | AwaitPlan (i -> Plan i o r)

deriving instance Functor (Plan i o)

instance Applicative (Plan i o) where
  pure = TerminatePlan
  l <*> r =
    case l of
      YieldPlan lo lNext ->
        YieldPlan lo $ lNext <*> r
      AwaitPlan lAwait ->
        AwaitPlan $ \i -> lAwait i <*> r
      TerminatePlan lr ->
        eliminateR r
        where
          eliminateR = \case
            TerminatePlan rr ->
              TerminatePlan (lr rr)
            YieldPlan ro rNext ->
              YieldPlan ro $ eliminateR rNext
            AwaitPlan rk ->
              AwaitPlan $ \i -> eliminateR $ rk i

instance Monad (Plan i o) where
  return = pure
  l >>= k =
    case l of
      TerminatePlan lr ->
        k lr
      YieldPlan lo lNext ->
        YieldPlan lo $ lNext >>= k
      AwaitPlan lAwait ->
        AwaitPlan $ \i -> lAwait i >>= k

await :: Plan i o i
await =
  AwaitPlan TerminatePlan

yield :: o -> Plan i o ()
yield output =
  YieldPlan output $ TerminatePlan ()

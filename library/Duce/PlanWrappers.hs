-- |
-- Wrappers for composition.
module Duce.PlanWrappers where

import Duce.Plan
import Duce.Prelude
import qualified StrictList

-- *

newtype Transducer i o = Transducer (Plan i o ())

-- | Provides for parallel composition.
instance Semigroup (Transducer i o) where
  Transducer lPlan <> Transducer rPlan =
    Transducer $ eliminate lPlan rPlan
    where
      eliminate lPlan rPlan = case lPlan of
        YieldPlan lo lPlan ->
          YieldPlan lo $ eliminate lPlan rPlan
        AwaitPlan lAwait ->
          AwaitPlan $ \i ->
            let eliminateR = \case
                  AwaitPlan rAwait ->
                    eliminate (lAwait i) (rAwait i)
                  YieldPlan ro rPlan ->
                    YieldPlan ro $ eliminateR rPlan
             in eliminateR rPlan

instance Monoid (Transducer i o) where
  mempty =
    Transducer skip
  mappend =
    (<>)

instance Functor (Transducer i) where
  fmap f (Transducer plan) =
    Transducer (mapOutput f plan)

-- *

newtype Reducer i r = Reducer (Plan i Void r)

-- *

newtype Producer o = Producer (Plan Void o ())

module Duce.Moore where

import Data.Machine.Moore
import qualified Duce.Plan as Plan
import Duce.Prelude

-- *

plan :: Plan.Plan i Void r -> Moore i r
plan plan = loop
  where
    loop = eliminate plan
    eliminate = \case
      Plan.TerminatePlan r ->
        error "TODO"

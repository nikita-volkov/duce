module Duce.Test.Suites.Transducer where

import Duce.Transducer
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

tests =
  [ testCase "Discretisation" $
      let tx = discretise 10 fst snd
          input =
            [ (3, 0),
              (13, 3),
              (23, 4),
              (24, 5),
              (54, 6),
              (63, 7)
            ]
          expected =
            [0, 3, 5, 5, 5, 6]
          actual =
            reverse $ fst $ eliminateUntilAwaiting $ consumeList input tx
       in assertEqual "" expected actual
  ]

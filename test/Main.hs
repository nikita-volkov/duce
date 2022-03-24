import qualified Duce.Test.Suites.Transducer as TransducerSuite
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Prelude

main =
  defaultMain . testGroup "All" $
    [ testGroup "Transducer" TransducerSuite.tests
    ]

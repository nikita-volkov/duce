module Duce.Reducer
  ( Reducer (..),
    transduce,
    head,
  )
where

import qualified Control.Comonad as Comonad
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Types as Atto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Vector.Generic as Vec
import Duce.Defs
import Duce.Prelude hiding (concat, drop, dropWhile, either, find, foldl, head, null, par, product, seq, sum, take, takeWhile)
import qualified Duce.Prelude as Prelude
import qualified Duce.Text as Text
import qualified Duce.Vector as Vector
import qualified StrictList

transduce :: Transducer a b -> Reducer b o -> Reducer a o
transduce =
  eliminateReducer
  where
    eliminateReducer tx =
      \case
        AwaitingReducer reducerAwaiter ->
          eliminateTransducer reducerAwaiter tx
        TerminatedReducer o ->
          TerminatedReducer o
    eliminateTransducer reducerAwaiter =
      \case
        EmittingTransducer b nextTx ->
          eliminateReducer nextTx (reducerAwaiter b)
        AwaitingTransducer transducerAwaiter ->
          AwaitingReducer $ eliminateTransducer reducerAwaiter . transducerAwaiter

-- |
-- Await for the first value and terminate with it.
head :: Reducer a a
head =
  AwaitingReducer TerminatedReducer

module Duce.Reducer
(
  Reducer(..),
  transduce,
)
where

import Duce.Prelude hiding (par, seq, foldl, sum, product, take, drop, concat, takeWhile, dropWhile, either, null, head, find)
import Duce.Defs
import qualified Duce.Prelude as Prelude
import qualified Data.Vector.Generic as Vec
import qualified Control.Comonad as Comonad
import qualified Data.Attoparsec.Types as Atto
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Duce.Text as Text
import qualified Duce.Vector as Vector
import qualified Data.Text as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified StrictList
import qualified Text.Builder as TextBuilder


transduce :: Transducer a b -> Reducer b o -> Reducer a o
transduce =
  eliminateReducer
  where
    eliminateReducer tx =
      \ case
        AwaitingReducer reducerAwaiter ->
          eliminateTransducer reducerAwaiter tx
        TerminatedReducer o ->
          TerminatedReducer o
    eliminateTransducer reducerAwaiter =
      \ case
        EmittingTransducer b nextTx ->
          eliminateReducer nextTx (reducerAwaiter b)
        AwaitingTransducer transducerAwaiter ->
          AwaitingReducer $ eliminateTransducer reducerAwaiter . transducerAwaiter

module Duce.Reducer
  ( Reducer (..),
    transduce,
    head,

    -- *
    reduceFile,
    reduceLzmaFile,
  )
where

import qualified Conduit
import qualified Control.Comonad as Comonad
import qualified Data.Attoparsec.ByteString as AttoByteString
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Attoparsec.Types as Atto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Conduit.Lzma as LzmaConduit
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Unsafe as Text
import qualified Data.Vector.Generic as GenericVector
import Duce.Core.Reducer
import Duce.Core.Transducer
import Duce.Prelude hiding (concat, drop, dropWhile, either, find, foldl, head, null, par, product, seq, sum, take, takeWhile)
import qualified Duce.Prelude as Prelude
import qualified Duce.Text as Text
import qualified StrictList
import qualified VectorExtras.Accumulator as VectorAccumulator

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

-- *

reducifyMealy :: Mealy a b -> Reducer b c -> Reducer a c
reducifyMealy (Mealy runMealy) = \case
  AwaitingReducer nextReducer -> AwaitingReducer $ \a -> case runMealy a of
    (b, nextMealy) -> reducifyMealy nextMealy (nextReducer b)
  TerminatedReducer c -> TerminatedReducer c

reducifyMoore :: Moore a b -> Reducer b c -> Reducer a c
reducifyMoore (Moore b nextMoore) = \case
  AwaitingReducer nextReducer -> AwaitingReducer $ \a -> reducifyMoore (nextMoore a) (nextReducer b)
  TerminatedReducer c -> TerminatedReducer c

-- *

-- |
-- Await for the first value and terminate with it.
head :: Reducer a a
head =
  AwaitingReducer TerminatedReducer

vector :: GenericVector.Vector v a => Reducer a (v a)
vector =
  go VectorAccumulator.init
  where
    go !acc =
      error "TODO"

-- *

reduceFile :: FilePath -> Reducer ByteString o -> IO (Maybe o)
reduceFile path reducer =
  Conduit.withSourceFile path $ \source ->
    Conduit.runConduit $ source Conduit..| toConduitSink reducer

reduceLzmaFile :: FilePath -> Reducer ByteString o -> IO (Maybe o)
reduceLzmaFile path reducer =
  Conduit.withSourceFile path $ \source ->
    Conduit.runConduit $ source Conduit..| LzmaConduit.decompress Nothing Conduit..| toConduitSink reducer

-- *

toConduitSink :: Reducer i o -> Conduit.ConduitT i Void IO (Maybe o)
toConduitSink = \case
  AwaitingReducer awaiter ->
    Conduit.await >>= \case
      Just i -> toConduitSink (awaiter i)
      Nothing -> pure Nothing
  TerminatedReducer res ->
    pure (Just res)

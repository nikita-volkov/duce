module Duce.Fold where

import qualified Conduit
import Control.Foldl (Fold (..))
import qualified Duce.Plan as Plan
import Duce.Prelude
import qualified Duce.Transducer as Transducer
import qualified Duce.Util.Conduit as ConduitUtil

-- *

transduce :: Transducer.Transducer i o -> Fold o r -> Fold i r
transduce tx (Fold progress start finish) =
  Fold progress' start' finish'
  where
    start' =
      eliminate start tx

    progress' (await, state) input =
      eliminate state $ await input

    finish' (_, state) =
      finish state

    eliminate state = \case
      Transducer.EmittingTransducer output tx ->
        eliminate (progress state output) tx
      Transducer.AwaitingTransducer await ->
        (await, state)

-- *

foldFile :: FilePath -> Fold ByteString r -> IO r
foldFile path fold =
  ConduitUtil.processFile path $ toConduit fold

foldLzmaFile :: FilePath -> Fold ByteString r -> IO r
foldLzmaFile path fold =
  ConduitUtil.processLzmaFile path $ toConduit fold

-- *

toConduit :: Monad m => Fold i r -> Conduit.ConduitT i o m r
toConduit (Fold progress start finish) =
  go start
  where
    go !state =
      Conduit.await >>= \case
        Just i -> go (progress state i)
        Nothing -> pure $ finish state

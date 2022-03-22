module Duce.Defs where

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
import Duce.Prelude hiding (concat, drop, dropWhile, either, find, foldl, head, null, par, product, seq, sum, take, takeWhile)
import qualified Duce.Prelude as Prelude
import qualified Duce.Text as Text
import qualified Duce.Vector as Vector
import qualified StrictList

-- *

-- |
-- Active reducer.
data Reducer i o
  = AwaitingReducer (i -> Reducer i o)
  | TerminatedReducer o

deriving instance Functor (Reducer i)

instance Applicative (Reducer i) where
  pure = TerminatedReducer
  (<*>) = ap

-- |
-- First feeds the left reducer until it terminates with an output value,
-- which is then used to get the next reducer,
-- which it then feeds the following inputs.
instance Monad (Reducer i) where
  return = pure
  (>>=) =
    \case
      AwaitingReducer await ->
        \k -> AwaitingReducer $ \i -> await i >>= k
      TerminatedReducer a ->
        ($ a)

-- *

data Transducer i o
  = -- | Awaiting next input.
    AwaitingTransducer (i -> Transducer i o)
  | -- | Emitting an output and producing the next state,
    -- which itself may be emitting yet another output.
    EmittingTransducer o (Transducer i o)

-- | Provides for parallel composition.
instance Semigroup (Transducer i o) where
  (<>) =
    build StrictList.Nil
    where
      build !oList =
        \case
          EmittingTransducer oL nextL ->
            build (StrictList.Cons oL oList) nextL
          AwaitingTransducer awaitL ->
            eliminateR oList
            where
              eliminateR !oList =
                \case
                  EmittingTransducer oR nextR ->
                    eliminateR (StrictList.Cons oR oList) nextR
                  AwaitingTransducer awaitR ->
                    foldl'
                      (flip EmittingTransducer)
                      (AwaitingTransducer (\i -> awaitL i <> awaitR i))
                      oList

instance Monoid (Transducer i o) where
  mempty =
    AwaitingTransducer (const mempty)
  mappend =
    (<>)

deriving instance Functor (Transducer i)

-- |
-- Drops out of sync emissions.
instance Applicative (Transducer a) where
  pure a = EmittingTransducer a $ pure a
  l <*> r = case l of
    EmittingTransducer lRes lNext -> case r of
      EmittingTransducer rRes rNext ->
        EmittingTransducer (lRes rRes) (lNext <*> rNext)
      AwaitingTransducer rAwait ->
        AwaitingTransducer $ \a -> dropEmissions lNext a <*> rAwait a
    AwaitingTransducer lAwait ->
      AwaitingTransducer $ \a -> lAwait a <*> dropEmissions r a
    where
      dropEmissions :: Transducer a b -> a -> Transducer a b
      dropEmissions = \case
        AwaitingTransducer await -> await
        EmittingTransducer _ next -> dropEmissions next

instance Alt (Transducer i) where
  (<!>) = (<>)

instance Category Transducer where
  id = AwaitingTransducer (\i -> EmittingTransducer i id)
  (.) = \case
    AwaitingTransducer bcAwaiter ->
      let eliminateAb = \case
            AwaitingTransducer abAwaiter -> AwaitingTransducer (\a -> eliminateAb (abAwaiter a))
            EmittingTransducer b abNext -> bcAwaiter b . abNext
       in eliminateAb
    EmittingTransducer c bcNext -> \ab -> EmittingTransducer c (bcNext . ab)

instance Arrow Transducer

instance ArrowChoice Transducer where
  (|||) = \case
    AwaitingTransducer lAwaiter ->
      let eliminateR = \case
            AwaitingTransducer rAwaiter ->
              AwaitingTransducer $ \case
                Left li -> lAwaiter li ||| AwaitingTransducer rAwaiter
                Right ri -> eliminateR (rAwaiter ri)
            EmittingTransducer o nextR -> EmittingTransducer o (eliminateR nextR)
       in eliminateR
    EmittingTransducer o nextL -> \r -> EmittingTransducer o (nextL ||| r)

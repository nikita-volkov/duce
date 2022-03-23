module Duce.Core.Reducer where

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
import Duce.Prelude hiding (concat, drop, dropWhile, either, find, foldl, head, null, par, product, seq, sum, take, takeWhile)
import qualified Duce.Prelude as Prelude
import qualified Duce.Text as Text
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

instance Profunctor Reducer where
  dimap map1 map2 = \case
    AwaitingReducer awaiter ->
      AwaitingReducer $ \i -> dimap map1 map2 $ awaiter $ map1 i
    TerminatedReducer res ->
      TerminatedReducer $ map2 res

instance Choice Reducer where
  right' = \case
    AwaitingReducer awaiter ->
      AwaitingReducer $ \case
        Right i -> right' $ awaiter i
        Left err -> TerminatedReducer $ Left err
    TerminatedReducer res ->
      TerminatedReducer $ Right res

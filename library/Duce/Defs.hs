module Duce.Defs
where

import Duce.Prelude hiding (par, seq, foldl, sum, product, take, drop, concat, takeWhile, dropWhile, either, null, head, find)
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


-- *
-------------------------

{-|
General reducer.
-}
data Reducer i o =
  AwaitingReducer (i -> Reducer i o) |
  TerminatedReducer o

deriving instance Functor (Reducer i)

{-|
Reducer wrapper, which provides instances for sequential composition.

Particularly this provides a monad instance,
which first feeds the left reducer until it terminates with an output value,
which is then used to get the next reducer,
which will be fed the following inputs.
-}
newtype SeqReducer i o = SeqReducer (Reducer i o)

deriving instance Functor (SeqReducer i)

instance Applicative (SeqReducer i) where
  pure = return
  (<*>) = ap

instance Monad (SeqReducer i) where
  return =
    SeqReducer . TerminatedReducer
  (>>=) =
    \ case
      SeqReducer (AwaitingReducer await) ->
        \ k -> SeqReducer $ AwaitingReducer $ \ i -> coerce (SeqReducer (await i) >>= k)
      SeqReducer (TerminatedReducer a) ->
        ($ a)


-- *
-------------------------

data Transducer i o =
  {-| Awaiting next input. -}
  AwaitingTransducer (i -> Transducer i o) |
  {-| Emitting an output and producing the next state,
      which itself may be emitting yet another output. -}
  EmittingTransducer o (Transducer i o)

{-| Provides for parallel composition. -}
instance Semigroup (Transducer i o) where
  (<>) =
    build StrictList.Nil
    where
      build !oList =
        \ case
          EmittingTransducer oL nextL ->
            build (StrictList.Cons oL oList) nextL
          AwaitingTransducer awaitL ->
            eliminateR oList
            where
              eliminateR !oList =
                \ case
                  EmittingTransducer oR nextR ->
                    eliminateR (StrictList.Cons oR oList) nextR
                  AwaitingTransducer awaitR ->
                    foldl' (flip EmittingTransducer)
                      (AwaitingTransducer (\ i -> awaitL i <> awaitR i))
                      oList

deriving instance Functor (Transducer i)

instance Alt (Transducer i) where
  (<!>) = (<>)

instance Category Transducer where
  id = AwaitingTransducer (\ i -> EmittingTransducer i id)
  (.) = \ case
    AwaitingTransducer bcAwaiter -> let
      eliminateAb = \ case
        AwaitingTransducer abAwaiter -> AwaitingTransducer (\ a -> eliminateAb (abAwaiter a))
        EmittingTransducer b abNext -> bcAwaiter b . abNext
      in eliminateAb
    EmittingTransducer c bcNext -> \ ab -> EmittingTransducer c (bcNext . ab)

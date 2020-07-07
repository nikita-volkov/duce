module Duce.Transducer
(
  Transducer(..),
  emitElements,
  each3,
  window,
  reduceEach,
  reduceWithOffset,
  quantize,
  quantizeWithMealy,
)
where

import Duce.Prelude hiding (par, seq, foldl, sum, product, take, drop, concat, takeWhile, dropWhile, either, null, head, find)
import Duce.Defs
import qualified StrictList
import qualified Data.Text as Text
import qualified Deque.Strict as Deque
import qualified Data.Map.Strict as Map
import qualified Duce.Util.Multimap as Multimap


{-|
Emit all elements from each input.
-}
emitElements :: Foldable f => Transducer (f o) o
emitElements =
  AwaitingTransducer $
  foldr EmittingTransducer emitElements

{-| Window on each 3 consecutive elements. -}
{-# INLINE each3 #-}
each3 :: (i -> i -> i -> o) -> Transducer i o
each3 cont =
  none
  where
    none =
      AwaitingTransducer one
    one i1 =
      AwaitingTransducer (two i1)
    two i1 i2 =
      AwaitingTransducer (three i1 i2)
    three i1 i2 i3 =
      EmittingTransducer
        (cont i1 i2 i3)
        (AwaitingTransducer (three i2 i3))

window :: Int {-^ Window size -} -> (Deque i -> o) -> Transducer i o
window windowSize proj =
  preparing windowSize empty
  where
    preparing missing deque =
      if missing > 0
        then AwaitingTransducer (\ i -> preparing (pred missing) (Deque.snoc i deque))
        else emitting deque
    emitting deque =
      case Deque.uncons deque of
        Just (h, t) ->
          EmittingTransducer (proj (Deque.cons h t)) (awaitingOne t)
        Nothing ->
          error "Bug"
    awaitingOne deque =
      AwaitingTransducer (\ i -> emitting (Deque.snoc i deque))

{-|
Launches a new branch of the reducer on each new input,
feeding all branches until they terminate with outputs.

Useful for windowing on consistently distanced series,
e.g., time-series with input data for every second.
To transform your data to that standard,
consider using quantization.
-}
reduceEach :: Reducer a b -> Transducer a b
reduceEach initialReducer =
  case initialReducer of
    TerminatedReducer o ->
      EmittingTransducer o ignoring
      where
        ignoring =
          AwaitingTransducer $ const ignoring
    AwaitingReducer initialAwaiter ->
      awaiting []
      where
        awaiting fedAwaiters =
          AwaitingTransducer $ \ i -> feedingAwaiters i [] (reverse (initialAwaiter : fedAwaiters))
          where
            feedingAwaiters i fedAwaiters =
              \ case
                unfedAwaiter : unfedAwaiters ->
                  case unfedAwaiter i of
                    AwaitingReducer fedAwaiter ->
                      feedingAwaiters i (fedAwaiter : fedAwaiters) unfedAwaiters
                    TerminatedReducer o ->
                      EmittingTransducer o $
                      feedingAwaiters i fedAwaiters unfedAwaiters
                _ -> awaiting fedAwaiters

reduceWithOffset :: Int -> Reducer a b -> Transducer a b
reduceWithOffset offset initialReducer =
  case initialReducer of
    TerminatedReducer o ->
      EmittingTransducer o ignoring
      where
        ignoring =
          AwaitingTransducer $ const ignoring
    AwaitingReducer initialAwaiter ->
      addingAwaiter []
      where
        addingAwaiter reversedAwaiters =
          awaiting offset (initialAwaiter : reversedAwaiters)
        awaiting currentOffset reversedAwaiters =
          AwaitingTransducer $ \ i ->
            dispatching currentOffset i (reverse reversedAwaiters)
        dispatching currentOffset i =
          eliminatingAwaiters []
          where
            eliminatingAwaiters fedAwaiters =
              \ case
                unfedAwaiter : unfedAwaiters ->
                  case unfedAwaiter i of
                    AwaitingReducer fedAwaiter ->
                      eliminatingAwaiters (fedAwaiter : fedAwaiters) unfedAwaiters
                    TerminatedReducer o ->
                      EmittingTransducer o $
                      eliminatingAwaiters fedAwaiters unfedAwaiters
                _ ->
                  if currentOffset > 0
                    then awaiting (pred currentOffset) fedAwaiters
                    else addingAwaiter fedAwaiters

{-|
Quantizer of time-series.
-}
quantize ::
  {-|
  The starting timestamp of the series.
  All samples prior to it will be discarded.
  And if the actual stream starts from a later point in time
  then the output stream will begin with 'Nothing'-values,
  standing for the missing ones.

  A typical value to provide here is the timestamp of the first value of the input stream.
  -}
  Int ->
  {-|
  Quantization. Defines the time-window from which the quantized sample will be aggregated.
  -}
  Int ->
  {-|
  How to get the time from the input sample.
  -}
  (a -> Int) ->
  {-|
  How to convert the input into an aggregate.
  -}
  (a -> b) ->
  {-|
  How to merge a previous aggregate with the one extracted from a new sample.
  
  You can use this function to average the values or simply select the first one.

  Naturally, this function is only applied if there's more than one sample collected
  in the quantization window.
  -}
  (b -> b -> b) ->
  {-|
  How to extract the output sample from the aggregate.
  This may also be useful for getting rid of the time information,
  since it becomes redundant after quantization.
  -}
  (b -> c) ->
  {-|
  Quantizing transducer.
  -}
  Transducer a (Maybe c)
quantize startTime quantization getTime getAggregate merge getResult =
  skippingBeforeStartTime
  where
    skippingBeforeStartTime =
      AwaitingTransducer $ \ a ->
        let
          time = getTime a
          in if time < startTime
            then skippingBeforeStartTime
            else startingWindow (startTime + quantization) time (getAggregate a)
    startingWindow endTime time b =
      if time < endTime
        then
          aggregatingWindow endTime b
        else
          let
            count = div (time - endTime) quantization
            newEndTime = endTime + count * quantization
            in
              foldr id (aggregatingWindow newEndTime b) $
              replicate count (EmittingTransducer Nothing)
    aggregatingWindow endTime !b =
      AwaitingTransducer $ \ a ->
        let
          time = getTime a
          in if time < endTime
            then
              aggregatingWindow endTime (merge b (getAggregate a))
            else
              EmittingTransducer (Just (getResult b)) $
              startingWindow (endTime + quantization) time (getAggregate a)

{-|
Quantizer of time-series.
-}
quantizeWithMealy ::
  {-|
  The starting timestamp of the series.
  All samples prior to it will be discarded.
  And if the actual stream starts from a later point in time
  then the output stream will begin with 'Nothing'-values,
  standing for the missing ones.

  A typical value to provide here is the timestamp of the first value of the input stream.
  -}
  Int ->
  {-|
  Quantization. Defines the time-window from which the quantized sample will be aggregated.
  -}
  Int ->
  {-|
  How to get the time from the input sample.
  -}
  (a -> Int) ->
  {-|
  Mealy machine, which specifies how to reduce the values of a quantized period,
  into a single value.
  
  Only the latest output of the machine will be used and forced.
  -}
  Mealy a b ->
  {-|
  Quantizing transducer.
  -}
  Transducer a (Maybe b)
quantizeWithMealy startTime quantization getTime initialMealy =
  skippingBeforeStartTime
  where
    skippingBeforeStartTime =
      AwaitingTransducer $ \ a ->
        let
          time = getTime a
          in if time < startTime
            then skippingBeforeStartTime
            else startingWindow (startTime + quantization) time (runMealy initialMealy a)
    startingWindow endTime time state =
      if time < endTime
        then
          aggregatingWindow endTime state
        else
          let
            count = div (time - endTime) quantization
            newEndTime = endTime + count * quantization
            in
              foldr id (aggregatingWindow newEndTime state) $
              replicate count (EmittingTransducer Nothing)
    aggregatingWindow endTime (b, mealy) =
      AwaitingTransducer $ \ a ->
        let
          time = getTime a
          in if time < endTime
            then
              aggregatingWindow endTime (runMealy mealy a)
            else
              EmittingTransducer (Just $! b) $
              startingWindow (endTime + quantization) time (runMealy initialMealy a)

sort :: Ord k => Int -> (a -> k) -> Transducer a a
sort cacheSize getKey =
  fillingCache cacheSize Multimap.empty
  where
    fillingCache missing =
      if missing > 0
        then
          awaiting (fillingCache (pred missing))
        else
          filled
    awaiting cont map =
      AwaitingTransducer $ \ a ->
        cont (Multimap.insert (getKey a) a map)
    awaitingOne =
      awaiting filled
    filled map =
      case Multimap.minView map of
        Just (a, newMap) ->
          EmittingTransducer a $
          awaitingOne newMap
        Nothing ->
          id

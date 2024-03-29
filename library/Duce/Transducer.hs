module Duce.Transducer
  ( Transducer (..),
    consumeOne,
    consumeList,
    eliminateUntilAwaiting,
    emitElements,
    each,
    each3,
    scan,
    reduce,
    batch,
    take,
    window,
    reduceEach,
    reduceWithOffset,
    quantize,
    quantizeWithMealy,
    quantizeWithMoore,
    timestamp,
    transducifyMealy,
    transducifyMoore,
    discretise,
    decodeUsingCereal,

    -- *
    plan,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Serialize.Get as CerealGet
import qualified Data.Text as Text
import qualified Deque.Strict as Deque
import Duce.Core.Reducer
import Duce.Core.Transducer
import qualified Duce.Plan as Plan
import Duce.Prelude hiding (concat, drop, dropWhile, either, find, foldl, head, map, null, par, product, seq, sum, take, takeWhile)
import qualified Duce.Util.Multimap as Multimap
import qualified StrictList

consumeOne :: i -> Transducer i o -> Transducer i o
consumeOne i = \case
  AwaitingTransducer awaiter -> awaiter i
  EmittingTransducer o next -> EmittingTransducer o (consumeOne i next)

consumeList :: [i] -> Transducer i o -> Transducer i o
consumeList inputs = \case
  AwaitingTransducer awaiter -> case inputs of
    i : iTail -> consumeList iTail (awaiter i)
    _ -> AwaitingTransducer awaiter
  EmittingTransducer o next -> EmittingTransducer o (consumeList inputs next)

-- |
-- Collect all outputs as reverse-list until an input is requested.
eliminateUntilAwaiting :: Transducer i o -> ([o], i -> Transducer i o)
eliminateUntilAwaiting = build []
  where
    build oList = \case
      AwaitingTransducer awaiter -> (oList, awaiter)
      EmittingTransducer o next -> build (o : oList) next

emitFoldable :: Foldable f => f o -> Transducer i o -> Transducer i o
emitFoldable = flip (foldr EmittingTransducer)

-- |
-- Emit all elements from each input.
emitElements :: Foldable f => Transducer (f o) o
emitElements =
  AwaitingTransducer $
    foldr EmittingTransducer emitElements

each :: (i -> o) -> Transducer i o
each f =
  AwaitingTransducer (\i -> EmittingTransducer (f i) (each f))

-- | Window on each 3 consecutive elements.
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

scan :: (b -> a -> b) -> b -> Transducer a b
scan step =
  fix $ \loop !b ->
    EmittingTransducer b (AwaitingTransducer (\a -> loop (step b a)))

-- |
-- Construct transducer from reducer.
--
-- Allows to apply reducer\'s monadic API,
-- where 'Duce.Reducer.head' can be used for awaiting for next input
-- and 'return' - for yielding.
--
-- E.g., 'each2' can be defined thus:
--
-- >each2 :: (i -> i -> o) -> Transducer i o
-- >each2 cont =
-- >  reduce $ do
-- >    a <- Reducer.head
-- >    b <- Reducer.head
-- >    return (cont a b)
--
-- Please notice though that this is less powerful
-- than direct declaration of transducer,
-- since it reinitializes the reducer after each termination,
-- thus not allowing you to thread state.
reduce :: Reducer a b -> Transducer a b
reduce initialReducer =
  eliminateReducer initialReducer
  where
    eliminateReducer =
      \case
        AwaitingReducer nextReducerByInput ->
          AwaitingTransducer (eliminateReducer . nextReducerByInput)
        TerminatedReducer output ->
          EmittingTransducer output (eliminateReducer initialReducer)

batch :: Int -> Moore i o -> Transducer i o
batch amount moore =
  counting amount moore
  where
    counting amountLeft (Moore o nextMooreByInput) =
      if amountLeft > 0
        then AwaitingTransducer $ \i ->
          counting (pred amountLeft) (nextMooreByInput i)
        else EmittingTransducer o (counting amount moore)

take :: Int -> Transducer i i
take =
  counting
  where
    counting amountLeft =
      if amountLeft > 0
        then AwaitingTransducer $ \i ->
          EmittingTransducer i $
            counting (pred amountLeft)
        else finished
    finished =
      AwaitingTransducer (const finished)

window ::
  -- | Window size
  Int ->
  (Deque i -> o) ->
  Transducer i o
window windowSize proj =
  preparing windowSize empty
  where
    preparing missing deque =
      if missing > 0
        then AwaitingTransducer (\i -> preparing (pred missing) (Deque.snoc i deque))
        else emitting deque
    emitting deque =
      case Deque.uncons deque of
        Just (h, t) ->
          EmittingTransducer (proj (Deque.cons h t)) (awaitingOne t)
        Nothing ->
          error "Bug"
    awaitingOne deque =
      AwaitingTransducer (\i -> emitting (Deque.snoc i deque))

-- |
-- Launches a new branch of the reducer on each new input,
-- feeding all branches until they terminate with outputs.
--
-- Useful for windowing on consistently distanced series,
-- e.g., time-series with input data for every second.
-- To transform your data to that standard,
-- consider using quantization.
reduceEach :: Reducer a b -> Transducer a b
reduceEach initialReducer =
  case initialReducer of
    TerminatedReducer o ->
      each (const o)
    AwaitingReducer initialAwaiter ->
      awaiting []
      where
        awaiting fedAwaiters =
          AwaitingTransducer $ \i -> feedingAwaiters i [] (reverse (initialAwaiter : fedAwaiters))
          where
            feedingAwaiters i fedAwaiters =
              \case
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
          AwaitingTransducer $ \i ->
            dispatching currentOffset i (reverse reversedAwaiters)
        dispatching currentOffset i =
          eliminatingAwaiters []
          where
            eliminatingAwaiters fedAwaiters =
              \case
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

-- |
-- Quantizer of time-series.
quantize ::
  -- |
  --  The starting timestamp of the series.
  --  All samples prior to it will be discarded.
  --  And if the actual stream starts from a later point in time
  --  then the output stream will begin with 'Nothing'-values,
  --  standing for the missing ones.
  --
  --  A typical value to provide here is the timestamp of the first value of the input stream.
  Int ->
  -- |
  --  Quantization. Defines the time-window from which the quantized sample will be aggregated.
  Int ->
  -- |
  --  How to get the time from the input sample.
  (a -> Int) ->
  -- |
  --  How to convert the input into an aggregate.
  (a -> b) ->
  -- |
  --  How to merge a previous aggregate with the one extracted from a new sample.
  --
  --  You can use this function to average the values or simply select the first one.
  --
  --  Naturally, this function is only applied if there's more than one sample collected
  --  in the quantization window.
  (b -> b -> b) ->
  -- |
  --  How to extract the output sample from the aggregate.
  --  This may also be useful for getting rid of the time information,
  --  since it becomes redundant after quantization.
  (b -> c) ->
  -- |
  --  Quantizing transducer.
  Transducer a (Maybe c)
quantize startTime quantization getTime getAggregate merge getResult =
  skippingBeforeStartTime
  where
    skippingBeforeStartTime =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < startTime
              then skippingBeforeStartTime
              else startingWindow (startTime + quantization) time (getAggregate a)
    startingWindow endTime time b =
      if time < endTime
        then aggregatingWindow endTime b
        else
          let count = div (time - endTime) quantization
              newEndTime = endTime + count * quantization
           in foldr id (aggregatingWindow newEndTime b) $
                replicate count (EmittingTransducer Nothing)
    aggregatingWindow endTime !b =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < endTime
              then aggregatingWindow endTime (merge b (getAggregate a))
              else
                EmittingTransducer (Just (getResult b)) $
                  startingWindow (endTime + quantization) time (getAggregate a)

-- |
-- Quantizer of time-series.
quantizeWithMealy ::
  -- |
  --  The starting timestamp of the series.
  --  All samples prior to it will be discarded.
  --  And if the actual stream starts from a later point in time
  --  then the output stream will begin with 'Nothing'-values,
  --  standing for the missing ones.
  --
  --  A typical value to provide here is the timestamp of the first value of the input stream.
  Int ->
  -- |
  --  Quantization. Defines the time-window from which the quantized sample will be aggregated.
  Int ->
  -- |
  --  How to get the time from the input sample.
  (a -> Int) ->
  -- |
  --  Mealy machine, which specifies how to reduce the values of a quantized period,
  --  into a single value.
  --
  --  Only the latest output of the machine will be used and forced.
  Mealy a b ->
  -- |
  --  Quantizing transducer.
  Transducer a (Maybe b)
quantizeWithMealy startTime quantization getTime initialMealy =
  skippingBeforeStartTime
  where
    skippingBeforeStartTime =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < startTime
              then skippingBeforeStartTime
              else startingWindow (startTime + quantization) time (runMealy initialMealy a)
    startingWindow endTime time state =
      if time < endTime
        then aggregatingWindow endTime state
        else
          let count = div (time - endTime) quantization
              newEndTime = endTime + count * quantization
           in foldr id (aggregatingWindow newEndTime state) $
                replicate count (EmittingTransducer Nothing)
    aggregatingWindow endTime (b, mealy) =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < endTime
              then aggregatingWindow endTime (runMealy mealy a)
              else
                EmittingTransducer (Just $! b) $
                  startingWindow (endTime + quantization) time (runMealy initialMealy a)

-- |
-- Quantizer of time-series.
quantizeWithMoore ::
  -- |
  --  The starting timestamp of the series.
  --  All samples prior to it will be discarded.
  --  And if the actual stream starts from a later point in time
  --  then the output stream will begin with 'Nothing'-values,
  --  standing for the missing ones.
  --
  --  A typical value to provide here is the timestamp of the first value of the input stream.
  Int ->
  -- |
  --  Quantization. Defines the time-window from which the quantized sample will be aggregated.
  Int ->
  -- |
  --  How to get the time from the input sample.
  (a -> Int) ->
  -- |
  --  Moore machine, which specifies how to reduce the values of a quantized period,
  --  into a single value.
  --
  --  Only the latest output of the machine will be used and forced.
  Moore a b ->
  -- |
  --  Quantizing transducer.
  Transducer a b
quantizeWithMoore startTime quantization getTime (Moore defaultB initialFeedA) =
  skippingBeforeStartTime
  where
    skippingBeforeStartTime =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < startTime
              then skippingBeforeStartTime
              else startingWindow (startTime + quantization) time (initialFeedA a)
    startingWindow endTime time state =
      if time < endTime
        then aggregatingWindow endTime state
        else
          let count = div (time - endTime) quantization
              newEndTime = endTime + count * quantization
           in foldr id (aggregatingWindow newEndTime state) $
                replicate count (EmittingTransducer defaultB)
    aggregatingWindow endTime (Moore b feedA) =
      AwaitingTransducer $ \a ->
        let time = getTime a
         in if time < endTime
              then aggregatingWindow endTime (feedA a)
              else
                EmittingTransducer b $
                  startingWindow (endTime + quantization) time (initialFeedA a)

timestamp :: Int -> Int -> (Int -> a -> b) -> Transducer a b
timestamp startTime interval pack =
  mapping startTime
  where
    mapping time =
      AwaitingTransducer $ \a ->
        EmittingTransducer (pack time a) $
          mapping (time + interval)

sort :: Ord k => Int -> (a -> k) -> Transducer a a
sort cacheSize getKey =
  fillingCache cacheSize Multimap.empty
  where
    fillingCache missing =
      if missing > 0
        then awaiting (fillingCache (pred missing))
        else filled
    awaiting cont map =
      AwaitingTransducer $ \a ->
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

-- |
-- A safer variation of 'sort',
-- which ensures that the output is sorted by discarding the values,
-- which arrive after the window's active minimal value becomes larger.
-- This is affected by the size of the window, which you specify as the parameter.
sortDiscarding :: Ord k => Int -> (a -> k) -> Transducer a a
sortDiscarding cacheSize getKey =
  fillingCache cacheSize Multimap.empty
  where
    fillingCache missing map =
      if missing > 0
        then AwaitingTransducer $ \a ->
          fillingCache (pred missing) (Multimap.insert (getKey a) a map)
        else filled map
    filled map =
      case Multimap.minViewWithKey map of
        Just ((k, a), newMap) ->
          EmittingTransducer a $
            awaitingOne k newMap
        Nothing ->
          id
    awaitingOne minimum map =
      AwaitingTransducer $ \a ->
        let key = getKey a
         in if key > minimum
              then filled (Multimap.insert key a map)
              else
                if key == minimum
                  then
                    EmittingTransducer a $
                      awaitingOne minimum map
                  else awaitingOne minimum map

-- |
-- Generalise a Mealy machine as a Transducer.
--
-- Mealy machine essentially is a stateful mapper.
-- It produces exactly one output per input,
-- so the stream does not change density.
transducifyMealy :: Mealy a b -> Transducer a b
transducifyMealy (Mealy runMealy) =
  let await a = case runMealy a of
        (b, next) -> EmittingTransducer b $ transducifyMealy next
   in AwaitingTransducer await

-- |
-- Generalise a Moore machine as a Transducer.
transducifyMoore :: Moore a b -> Transducer a b
transducifyMoore (Moore b next) =
  EmittingTransducer b $ AwaitingTransducer $ transducifyMoore . next

discretise :: Int -> (a -> Int) -> (a -> b) -> Transducer a b
discretise distance toPosition toOutput =
  AwaitingTransducer $
    discretiseStartingWith distance toPosition toOutput

discretiseStartingWith :: Int -> (a -> Int) -> (a -> b) -> a -> Transducer a b
discretiseStartingWith distance toPosition toOutput =
  await <$> (+ distance) . toPosition <*> toOutput
  where
    await endPosition lastOutput =
      AwaitingTransducer $ \input ->
        decide endPosition lastOutput (toPosition input) (toOutput input)
    decide endPosition lastOutput position output =
      if position < endPosition
        then await endPosition output
        else emit endPosition lastOutput position output
    emit endPosition lastOutput position output =
      EmittingTransducer lastOutput $
        decide (endPosition + distance) lastOutput position output

decodeUsingCereal :: CerealGet.Get a -> Transducer ByteString (Either Text a)
decodeUsingCereal dec =
  await (CerealGet.runGetPartial dec)
  where
    await cont =
      AwaitingTransducer (fromResult . cont)
    fromResult = \case
      CerealGet.Done res rem ->
        EmittingTransducer
          (Right res)
          (fromResult (CerealGet.runGetPartial dec rem))
      CerealGet.Partial cont ->
        await cont
      CerealGet.Fail err rem ->
        EmittingTransducer
          (Left (fromString err))
          (fromResult (CerealGet.runGetPartial dec rem))

-- *

-- |
-- Compile a plan.
--
-- Provides for monadic construction of transducers.
plan :: Plan.Plan i o () -> Transducer i o
plan plan = loop
  where
    loop =
      eliminate plan
    eliminate = \case
      Plan.TerminatePlan () ->
        loop
      Plan.YieldPlan output next ->
        EmittingTransducer output $ eliminate next
      Plan.AwaitPlan await ->
        AwaitingTransducer $ eliminate . await

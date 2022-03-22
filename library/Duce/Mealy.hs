module Duce.Mealy where

import Data.Machine.Mealy
import Duce.Prelude hiding (scan, scan1)

scan :: (acc -> inp -> acc) -> acc -> Mealy inp acc
scan step =
  go
  where
    go state =
      Mealy $ \input ->
        (state, go (step state input))

scan1 :: (a -> a -> a) -> Mealy a a
scan1 step =
  Mealy go
  where
    go output =
      (output, Mealy $ go . step output)

-- |
-- Change indicator.
change :: Eq a => Mealy a Bool
change =
  Mealy $ \first -> (False, changeWithFirst first)

changeWithFirst :: Eq a => a -> Mealy a Bool
changeWithFirst prev =
  Mealy $ \inp ->
    if inp == prev
      then (False, changeWithFirst inp)
      else (True, changeWithFirst inp)

-- |
-- Exponential moving average.
ema :: Double -> Mealy Double Double
ema multiplier =
  scan1 $ \prevEma val ->
    val * multiplier + prevEma * (1 - multiplier)

-- |
-- Exponential moving average as per the TradingView definition.
--
-- The Exponential Moving Average (EMA) is a specific type of moving average that
-- points towards the importance of the most recent data and information from
-- the market. The Exponential Moving Average is just like it’s name says - it’s
-- exponential, weighting the most recent prices more than the less recent
-- prices. The EMA can be compared and contrasted with the simple moving
-- average.
--
-- Although there are many options to choose from when considering the smoothing
-- factor, most opt for a value of 2. This value gives more credibility to the
-- most recent data points available. The more a trader increases the smoothing
-- factor value, the more influence the most recent data will have on the moving
-- average.
--
-- To calculate the EMA, follow this simple formula.
--
-- The Exponential Moving Average is equal to the closing price multiplied by the
-- multiplier, plus the EMA of the previous day and then multiplied by 1 minus
-- the multiplier.
--
-- EMA = Closing price • multiplier + EMA (previous day) • (1-multiplier)
--
-- For details see
-- - https://www.tradingview.com/support/solutions/43000592270-exponential-moving-average/
-- - https://www.tradingview.com/ideas/ema/
tradingViewEma :: Double -> Double -> Mealy Double Double
tradingViewEma smoothing length =
  ema $
    smoothing / succ length

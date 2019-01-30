{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (maximumBy)
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Control.Monad.Writer.Lazy (writer)

type Power = Integer
type Size = Integer
type SerialNo = Integer
type Point = (Integer, Integer)
type Corner = Point
type Cells = HashMap Point Integer

grid :: Size -> Point -> [Point]
grid size (x, y) = (,) <$> [x..x+(size - 1)] <*> [y..y+(size - 1)]

powerRemaining :: SerialNo -> Cells -> Point -> (Cells, Power)
powerRemaining sn cs p = case Map.lookup p cs of
                           Just power -> (cs, power)
                           Nothing -> (cachePower p cs) &&& id $ calculatePower sn p
  where cachePower p cs = flip (Map.insert p) cs

calculatePower :: SerialNo -> Point -> Power
calculatePower sn (x, y) = hundredsDigit (nonsense sn x y) - 5
  where nonsense sn x y = (((x + 10) * y) + sn) * (x + 10)
        hundredsDigit n = mod (div n 100) 10

gridPower :: SerialNo -> Cells -> [Point] -> (Cells, Power)
gridPower sn cs ps = foldr (accumulatePower sn) (cs, 0) ps

accumulatePower :: SerialNo -> Point -> (Cells, Power) -> (Cells, Power)
accumulatePower sn p (cs, power) = (+ power) <$> powerRemaining sn cs p

field :: [Point]
field = (,) <$> [1..297] <*> [1..297]

powerInEachGrid :: SerialNo -> Cells -> [(Corner, [Point])] -> (Cells, [(Corner, Power)])
powerInEachGrid sn cells gs = foldr (forEachGrid sn) (cells, []) gs
  where forEachGrid :: SerialNo ->
                       (Corner, [Point]) ->
                       (Cells, [(Corner, Power)]) ->
                       (Cells, [(Corner, Power)])
        forEachGrid sn (p, g) (cells, rs) = (:rs) . (,) p <$> gridPower sn cells g

powerInGrids :: SerialNo -> Size -> Cells -> [Point] -> (Cells, [(Corner, Power)])
powerInGrids sn size cells corners = powerInEachGrid sn cells $ (id &&& grid size) <$> corners

maxPowerGrid :: SerialNo -> Size -> Cells -> (Cells, (Corner, Power))
maxPowerGrid sn size cells = maximumBy powerLevel <$> powerInGrids sn size cells field
  where powerLevel (_, a) (_, b) = compare a b

day11 :: SerialNo -> Corner
day11 sn = fst . snd $ maxPowerGrid sn 3 Map.empty

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (foldl')
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Control.Monad.Writer.Lazy (writer)

type Power = Integer
type SerialNo = Integer
type Size = Integer
type Point = (Integer, Integer)
type Cells = HashMap (Integer, Point) Integer

data Problem = Problem { serialNo :: Integer
                       , size :: Integer
                       }
                       deriving (Show)

data GridResult = GridResult { corner :: Point
                             , gridSize :: Integer
                             , power :: Power
                             }
                             deriving (Show, Eq)

instance Ord GridResult where
    compare g1 g2 = compare (power g1) (power g2)

gridPower :: Problem -> Cells -> Point -> (Cells, Power)
gridPower pb@(Problem sn 1) cells pt = lookupOrCalculate pb cells pt (calculatePower sn)
gridPower pb@(Problem sn sz) cells pt | odd sz = lookupOrCalculate pb cells pt (recurLeadingEdge pb cells)
                                      | otherwise = lookupOrCalculate pb cells pt (recurBox pb cells)

lookupOrCalculate :: Problem -> Cells -> Point -> (Point -> Power) -> (Cells, Power)
lookupOrCalculate (Problem sn sz) cells pt calc = case Map.lookup (sz, pt) cells of
                                                      Just power -> (cells, power)
                                                      Nothing -> cachePower sz cells pt $ calc pt

calculatePower :: SerialNo -> Point -> Power
calculatePower sn (x, y) = hundredsDigit (nonsense sn x y) - 5
  where nonsense sn x y = (((x + 10) * y) + sn) * (x + 10)
        hundredsDigit n = mod (div n 100) 10

cachePower :: Size -> Cells -> Point -> Power -> (Cells, Power)
cachePower sz cells pt = insertCache (sz, pt) cells &&& id
  where insertCache p cells = flip (Map.insert p) cells

recurLeadingEdge :: Problem -> Cells -> Point -> Power
recurLeadingEdge pb@(Problem sn sz) cells pt = (leadingEdge pb cells pt) +
                                          (snd $ gridPower (Problem sn (sz - 1))
                                                           cells
                                                           pt)
  where leadingEdge :: Problem -> Cells -> Point -> Power
        leadingEdge (Problem sn sz) cells (x, y) =
          let
            xMax = (x + sz - 1)
            yMax = (y + sz - 1)
            xRange = [x..xMax]
            yRange = [y..(yMax - 1)] -- don't double count the corner
            forwardEdge = (xMax,) <$> yRange
            bottomEdge = (, yMax) <$> xRange
          in
            sum $
            snd <$>
            gridPower (Problem sn 1) cells <$>
            (forwardEdge ++ bottomEdge)

recurBox :: Problem -> Cells -> Point -> Power
recurBox (Problem sn sz) cells (x, y) =
    let
      delta = div sz 2
      corners = [(x, y), (x + delta, y), (x, y + delta), (x + delta, y + delta)]
    in
      sum $ snd . (gridPower (Problem sn delta) cells) <$> corners

corners :: Size -> [Point]
corners sz =
    let maxBound = 300 - sz + 1
    in (,) <$> [1..maxBound] <*> [1..maxBound]

sizes :: [Size]
sizes = [1..300]

day11 :: SerialNo -> Point
day11 sn = corner . snd $ problemMax (Problem sn 3) Map.empty

day11p2 :: SerialNo -> (Size, Point)
day11p2 sn =
    let
      problems = Problem sn <$> sizes
      initial = problemMax (head problems) Map.empty
    in
      (gridSize &&& corner) $ snd $ foldl' maxPower initial (tail problems)
  where maxPower :: (Cells, GridResult) -> Problem -> (Cells, GridResult)
        maxPower (cells, gr) pb =
          let
            (cells', gr') = problemMax pb cells
          in
            (cells', max gr gr')

problemMax :: Problem -> Cells -> (Cells, GridResult)
problemMax pb cells =
    let
      cs = corners (size pb)
      initial = GridResult (head cs) (size pb) <$>
                gridPower pb cells (head cs)
    in
      foldl' (maxPower pb) initial (tail cs)
  where maxPower :: Problem -> (Cells, GridResult) -> Point -> (Cells, GridResult)
        maxPower pb (cells, gr) pt =
          let
            (cells', power) = gridPower pb cells pt
          in
            (cells', max gr (GridResult pt (size pb) power))

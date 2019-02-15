{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.Bifunctor (first, second)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (foldl', maximumBy)
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Control.Monad.Writer.Lazy (writer)

type Power = Integer
type SerialNo = Integer
type Size = Integer
type Point = (Integer, Integer)
type Dim = (Integer, Integer)
type Cells = HashMap (Dim, Point) Integer

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
gridPower pb@(Problem sn 1) cells pt = lookupOrCalculate pb cells pt (calculatePower sn cells)
gridPower pb@(Problem sn sz) cells pt = lookupOrCalculate pb cells pt (recurLeadingEdge pb cells)

lookupOrCalculate :: Problem -> Cells -> Point -> (Point -> (Cells, Power)) -> (Cells, Power)
lookupOrCalculate (Problem sn sz) cells pt calc = case Map.lookup ((sz, sz), pt) cells of
                                                      Just power -> (cells, power)
                                                      Nothing -> cachePower (sz, sz) pt $ calc pt

calculatePower :: SerialNo -> Cells -> Point -> (Cells, Power)
calculatePower sn cells (x, y) = (cells, hundredsDigit (nonsense sn x y) - 5)
  where nonsense sn x y = (((x + 10) * y) + sn) * (x + 10)
        hundredsDigit n = mod (div n 100) 10

cachePower :: Dim -> Point -> (Cells, Power) -> (Cells, Power)
cachePower dim pt (cells, pwr) = insertCache (dim, pt) cells &&& id $ pwr
  where insertCache p cells = flip (Map.insert p) cells

recurLeadingEdge :: Problem -> Cells -> Point -> (Cells, Power)
recurLeadingEdge pb@(Problem sn sz) cells pt =
  let
    (cells', edgePower) = leadingEdge pb cells pt
  in
    ( cells'
    , (fromJust $ Map.lookup ((sz - 1, sz - 1), pt) cells') + edgePower
    )
  where leadingEdge :: Problem -> Cells -> Point -> (Cells, Power)
        leadingEdge (Problem sn sz) cells (x, y) =
          let
            xMax = (x + sz - 1)
            yMax = (y + sz - 1)
            bottomEdgeKey = ((sz - 1, 1), (x, yMax))
            bottomEdgePower = fromJust $ Map.lookup bottomEdgeKey cells
            forwardEdgeKey = ((1, sz - 1 ), (xMax, y))
            forwardEdgePower = fromJust $ Map.lookup forwardEdgeKey cells
            cornerKey = ((1, 1), (xMax, yMax))
            cornerPower = fromJust $ Map.lookup cornerKey cells
            cells' = Map.insert (first (first (+1)) bottomEdgeKey) (bottomEdgePower + cornerPower) $
                     Map.insert (first (second (+1)) forwardEdgeKey) (forwardEdgePower + cornerPower) $
                     cells
          in
            ( cells'
            , bottomEdgePower + forwardEdgePower + cornerPower
            )

corners :: Size -> [Point]
corners sz =
    let maxBound = 300 - sz + 1
    in tail $ (,) <$> [1..maxBound] <*> [1..maxBound]

sizes :: [Size]
sizes = [1..300]

day11 :: SerialNo -> Point
day11 sn =
    let
      initial =  problemMax (Problem sn 1) Map.empty
      (cells, _) = foldl' fillCells initial (Problem sn <$> [2, 3])
    in
      snd . fst $ maximumBy power $ filter threeByThree $ Map.toList cells
    where threeByThree = (== (3, 3)) . fst . fst
          power (_, p1) (_, p2) = compare p1 p2
          fillCells (cells, _) pb = problemMax pb cells

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
      initial = GridResult (1, 1) (size pb) <$>
                gridPower pb cells (1, 1)
    in
      foldl' (maxPower pb) initial (corners $ size pb)
  where maxPower :: Problem -> (Cells, GridResult) -> Point -> (Cells, GridResult)
        maxPower pb (cells, gr) pt =
          let
            (cells', power) = gridPower pb cells pt
          in
            (cells', max gr (GridResult pt (size pb) power))

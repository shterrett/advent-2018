{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Prelude hiding (lookup)
import qualified Data.Text as T
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List (foldl', maximumBy)
import Control.Arrow ((&&&))
import Control.Monad (foldM)
import Control.Monad.Writer.Lazy (writer)
import Debug.Trace

type Power = Integer
type SerialNo = Integer
type Size = Integer
type Point = (Integer, Integer)
type Dim = (Integer, Integer)
type Cells = HashMap (Dim, Point) Integer

data Cache = Cache { currentDim :: Cells
                   , prevDim :: Cells
                   }
                   deriving (Show, Eq)

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

newCache :: Cache
newCache = Cache Map.empty Map.empty

cycleCache :: Cache -> Cache
cycleCache (Cache current prev) = Cache Map.empty current

lookup :: (Dim, Point) -> Cache -> Power
lookup key (Cache current prev) = fromJust $ Map.lookup key prev

store :: (Dim, Point) -> Power -> Cache -> Cache
store key power c = c { currentDim = Map.insert key power (currentDim c) }

gridPower :: Problem -> Cache -> Point -> (Cache, Power)
gridPower pb@(Problem sn 1) cache pt = (cache, calculatePower sn pt)
gridPower pb@(Problem sn sz) cache pt = recurLeadingEdge pb cache pt

calculatePower :: SerialNo -> Point -> Power
calculatePower sn (x, y) = hundredsDigit (nonsense sn x y) - 5
  where nonsense sn x y = (((x + 10) * y) + sn) * (x + 10)
        hundredsDigit n = mod (div n 100) 10

recurLeadingEdge :: Problem -> Cache -> Point -> (Cache, Power)
recurLeadingEdge pb@(Problem sn sz) cache pt =
  let
    (cache', edgePower) = leadingEdge pb cache pt
    smallBlockPower = if sz - 1 == 1
                        then calculatePower sn pt
                        else lookup ((sz - 1, sz - 1), pt) cache'
    totalPower = smallBlockPower + edgePower
  in
    ( store ((sz, sz), pt) totalPower cache'
    , totalPower
    )
  where leadingEdge :: Problem -> Cache -> Point -> (Cache, Power)
        leadingEdge (Problem sn sz) cache (x, y) =
          let
            problem' = Problem sn (sz - 1)
            xMax = (x + sz - 1)
            yMax = (y + sz - 1)
            bottomEdgeKey = ((sz - 1, 1), (x, yMax))
            bottomEdgePower = edgePower problem' bottomEdgeKey cache
            forwardEdgeKey = ((1, sz - 1 ), (xMax, y))
            forwardEdgePower = edgePower problem' forwardEdgeKey cache
            cornerPower = calculatePower sn (xMax, yMax)
            cache' = store ((sz, 1), (x, yMax)) (bottomEdgePower + cornerPower) $
                     store ((1, sz), (xMax, y)) (forwardEdgePower + cornerPower) cache
          in
            ( cache'
            , bottomEdgePower + forwardEdgePower + cornerPower
            )
        edgePower :: Problem -> (Dim, Point) -> Cache -> Power
        edgePower (Problem sn sz) key@(_, pt) cache | sz == 1 = calculatePower sn pt
                                                    | otherwise = lookup key cache

corners :: Size -> [Point]
corners sz =
    let maxBound = 300 - sz + 1
    in tail $ (,) <$> [1..maxBound] <*> [1..maxBound]

sizes :: [Size]
sizes = [1..300]

day11 :: SerialNo -> Point
day11 sn =
    let
      initial = problemMax (Problem sn 1) newCache
      (cache, _) = foldl' fillCache initial (Problem sn <$> [2, 3])
    in
      snd . fst $ maximumBy power $ filter threeByThree $ Map.toList (currentDim cache)
    where threeByThree = (== (3, 3)) . fst . fst
          power (_, p1) (_, p2) = compare p1 p2
          fillCache (cache, _) pb = problemMax pb (cycleCache cache)

day11p2 :: SerialNo -> (Size, Point)
day11p2 sn =
    let
      problems = Problem sn <$> sizes
      initial = problemMax (head problems) newCache
    in
      (gridSize &&& corner) $ snd $ foldl' maxPower initial (tail problems)
  where maxPower :: (Cache, GridResult) -> Problem -> (Cache, GridResult)
        maxPower (cache, gr) pb =
          let
            (cache', gr') = problemMax pb (cycleCache cache)
          in
            (cache', max gr gr')

problemMax :: Problem -> Cache -> (Cache, GridResult)
problemMax pb cache =
    let
      initial = GridResult (1, 1) (size pb) <$>
                gridPower pb cache (1, 1)
    in
      foldl' (maxPower pb) initial (corners $ size pb)
  where maxPower :: Problem -> (Cache, GridResult) -> Point -> (Cache, GridResult)
        maxPower pb (cache, gr) pt =
          let
            (cache', power) = gridPower pb cache pt
          in
            (cache', max gr (GridResult pt (size pb) power))

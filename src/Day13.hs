{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Tuple (swap)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import qualified Data.Heap as Heap
import Data.Heap (MinHeap)
import Control.Monad (foldM)

type Point = (Int, Int)
data SegmentClass = Horizontal   -- -
                  | Vertical     -- |
                  | NTurn        -- /
                  | PTurn        -- \
                  | Intersection -- +
                  | Empty
                  deriving (Show, Eq)
data Direction = PosX
               | NegX
               | PosY
               | NegY
               deriving (Show, Eq)
data Turn = LeftTurn
          | Straight
          | RightTurn
          deriving (Show, Eq)
data Car = Car { position :: Point
               , heading :: Direction
               , nextTurn :: Turn
               }
               deriving (Show, Eq)

instance Ord Car where
    compare (Car p1 _ _) (Car p2 _ _) = compare (swap p1) (swap p2)
data Tracks = Tracks { segments :: HashMap Point SegmentClass
                     , occupied :: HashSet Point
                     , cars :: MinHeap Car
                     }
                     deriving (Show, Eq)

parseInput :: [T.Text] -> Tracks
parseInput lines =
    let
      points = parsePoints lines
      cars = parseCars lines
    in
      Tracks (Map.fromList points)
             (Set.fromList (position <$> cars))
             (Heap.fromList cars)
  where parsePoints lines = filter notEmpty $
                            labelPoints $
                            (fmap classifySegment) <$>
                            T.unpack <$>
                            lines
        parseCars lines = mapMaybe buildCar $
                          labelPoints $
                          T.unpack <$>
                          lines

classifySegment :: Char -> SegmentClass
classifySegment '-' = Horizontal
classifySegment '>' = Horizontal
classifySegment '<' = Horizontal
classifySegment '|' = Vertical
classifySegment '^' = Vertical
classifySegment 'v' = Vertical
classifySegment '/' = NTurn
classifySegment '\\' = PTurn
classifySegment '+' = Intersection
classifySegment ' ' = Empty

labelPoints :: [[a]] -> [((Int, Int), a)]
labelPoints rs = concat $ labelRow <$> zip [0, 1..] rs
  where labelRow (n, ps) = zip ((, n) <$> [0, 1..]) ps

buildCar :: (Point, Char) -> Maybe Car
buildCar (_, '-') = Nothing
buildCar (_, '|') = Nothing
buildCar (_, '/') = Nothing
buildCar (_, '\\') = Nothing
buildCar (_, '+') = Nothing
buildCar (_, ' ') = Nothing
buildCar (p, '>') = Just $ Car p PosX LeftTurn
buildCar (p, '<') = Just $ Car p NegX LeftTurn
buildCar (p, 'v') = Just $ Car p PosY LeftTurn
buildCar (p, '^') = Just $ Car p NegY LeftTurn

notEmpty :: (Point, SegmentClass) -> Bool
notEmpty (_, Empty) = False
notEmpty _ = True

tick :: Tracks -> Either Point Tracks
tick ts =
    let
      cs = Heap.toAscList $ cars ts
      ts' = ts { cars = Heap.empty }
    in
      (foldM moveCar ts' cs) >>= tick

moveCar :: Tracks -> Car -> Either Point Tracks
moveCar ts c =
    let
      occ = Set.delete (position c) (occupied ts)
      c' = updateCar (segments ts) c
    in
      if Set.member (position c') occ
        then Left (position c')
        else Right $ ts { occupied = Set.insert (position c') occ
                        , cars = Heap.insert c' (cars ts)
                        }

updateCar :: HashMap Point SegmentClass -> Car -> Car
updateCar ps c@(Car p _ _) = case Map.lookup p ps of
                                Nothing -> undefined
                                Just sc -> update sc c
  where update Horizontal (Car (x, y) PosX nt) = Car (x + 1, y) PosX nt
        update Horizontal (Car (x, y) NegX nt) = Car (x - 1, y) NegX nt
        update Vertical (Car (x, y) PosY nt) = Car (x, y + 1) PosY nt
        update Vertical (Car (x, y) NegY nt) = Car (x, y - 1) NegY nt
        update PTurn (Car (x, y) PosX nt) = Car (x, y + 1) PosY nt
        update PTurn (Car (x, y) PosY nt) = Car (x + 1, y) PosX nt
        update PTurn (Car (x, y) NegX nt) = Car (x, y - 1) NegY nt
        update PTurn (Car (x, y) NegY nt) = Car (x - 1, y) NegX nt
        update NTurn (Car (x, y) PosX nt) = Car (x, y - 1) NegY nt
        update NTurn (Car (x, y) PosY nt) = Car (x - 1, y) NegX nt
        update NTurn (Car (x, y) NegX nt) = Car (x, y + 1) PosY nt
        update NTurn (Car (x, y) NegY nt) = Car (x + 1, y) PosX nt
        update Intersection (Car (x, y) PosX LeftTurn) = Car (x, y - 1) NegY Straight
        update Intersection (Car (x, y) PosX Straight) = Car (x + 1, y) PosX RightTurn
        update Intersection (Car (x, y) PosX RightTurn) = Car (x, y + 1) PosY LeftTurn
        update Intersection (Car (x, y) NegX LeftTurn) = Car (x, y + 1) PosY Straight
        update Intersection (Car (x, y) NegX Straight) = Car (x - 1, y) NegX RightTurn
        update Intersection (Car (x, y) NegX RightTurn) = Car (x, y - 1) NegY LeftTurn
        update Intersection (Car (x, y) PosY LeftTurn) = Car (x + 1, y) PosX Straight
        update Intersection (Car (x, y) PosY Straight) = Car (x, y + 1) PosY RightTurn
        update Intersection (Car (x, y) PosY RightTurn) = Car (x - 1, y) NegX LeftTurn
        update Intersection (Car (x, y) NegY LeftTurn) = Car (x - 1, y) NegX Straight
        update Intersection (Car (x, y) NegY Straight) = Car (x, y - 1) NegY RightTurn
        update Intersection (Car (x, y) NegY RightTurn) = Car (x + 1, y) PosX LeftTurn

day13 :: [T.Text] -> T.Text
day13 lines = case tick (parseInput lines) of
                Left p -> T.pack $ show p
                Right _ -> "Something has gone terribly wrong"

tick' :: (Tracks, HashSet Point) -> (Tracks, HashSet Point)
tick' (ts, ps) =
    let
      cs = Heap.toAscList $ cars ts
      ts' = ts { cars = Heap.empty }
    in
      case cs of
        [c] -> (ts' { cars = Heap.singleton c }
               , Set.empty)
        cs' -> tick' $ moveAllCars (ts', Set.empty) cs'

moveAllCars :: (Tracks, HashSet Point) -> [Car] -> (Tracks, HashSet Point)
moveAllCars tp [] = tp
moveAllCars tp (c:cs) =
    let
      (ts, ps) = moveCar' tp c
    in
      moveAllCars (ts, ps) $ filter (uncrashed ps) cs

uncrashed :: HashSet Point -> Car -> Bool
uncrashed ps = (not . (flip Set.member) ps) . position

moveCar' :: (Tracks, HashSet Point) -> Car -> (Tracks, HashSet Point)
moveCar' (ts, ps) c =
    let
      occ = Set.delete (position c) (occupied ts)
      c' = updateCar (segments ts) c
      ps' = Set.insert (position c') ps
    in
      if Set.member (position c') occ
        then ( ts { occupied = Set.delete (position c') occ
                  , cars = Heap.filter (uncrashed ps') (cars ts)
                  }
             , ps')
        else ( ts { occupied = Set.insert (position c') occ
                  , cars = Heap.insert c' (cars ts)
                  }
             , ps)

day13p2 :: [T.Text] -> T.Text
day13p2 lines = fromMaybe "Something has gone terribly wrong" .
                (fmap $ T.pack . show . position) .
                Heap.viewHead .
                cars .
                fst .
                tick' .
                (,Set.empty) .
                parseInput $
                lines

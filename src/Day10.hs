{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import qualified Data.Text as T
import Text.Parsec
import Text.Parser.Token (integer, whiteSpace)
import Data.List (minimumBy, maximumBy)
import qualified Data.HashSet as Set
import Control.Arrow ((&&&))

data Point = Point { x_coord :: Integer, y_coord :: Integer }
           deriving (Show, Eq)
data Velocity = Velocity { delta_x :: Integer, delta_y :: Integer }
              deriving (Show, Eq)

parseInput :: [T.Text] -> Either ParseError [(Point, Velocity)]
parseInput = sequence . fmap (parse parseLine "" . T.unpack)

parseLine :: Parsec String () (Point, Velocity)
parseLine = (,) <$>
            (string "position=<" >> pointParser) <*>
            (char '>' >> whiteSpace >> string "velocity=<" >> velocityParser)
  where pointParser = (uncurry Point) <$> pairParser
        velocityParser = (uncurry Velocity) <$> pairParser

pairParser :: Parsec String () (Integer, Integer)
pairParser = (,) <$> (whiteSpace >> integer) <*> (char ',' >> whiteSpace >> integer)

step :: (Point, Velocity) -> (Point, Velocity)
step (p, v) = ( Point (x_coord p + delta_x v)
                      (y_coord p + delta_y v)
              , v
              )

boundingBox :: [Point] -> (Point, Point)
boundingBox = (mkPoint . bound min) &&& (mkPoint . bound max)
  where mkPoint = uncurry Point
        bound f = f . xs &&& f . ys
        min = minimumBy compare
        max = maximumBy compare
        xs = fmap x_coord
        ys = fmap y_coord

area :: (Point, Point) -> Integer
area = (uncurry (*)) . ((sideLength x_coord) &&& (sideLength y_coord))
    where sideLength s (p1, p2) = abs $ (s p1) - (s p2)

findMessage :: [(Point, Velocity)] -> (Integer, [(Point, Velocity)])
findMessage ps = findMessage' 0 (boundingBox $ fst <$> ps) ps

findMessage' :: Integer -> (Point, Point) -> [(Point, Velocity)] -> (Integer, [(Point, Velocity)])
findMessage' s box ps = let
    next = step <$> ps
    nextBox = boundingBox $ fst <$> next
  in
    if (area nextBox) > (area box)
      then (s, ps)
      else findMessage' (s + 1) nextBox next

day10 :: [T.Text] -> T.Text
day10 input = either (T.pack . show) (T.pack . show) $
              (fmap $ showMessage . (fmap fst)) . findMessage <$>
              parseInput input

showMessage :: [Point] -> String
showMessage ps =
    let
      markers = Set.fromList $ orderedPair <$> ps
      box = boundingBox ps
      (minX, minY) = orderedPair $ fst box
      (maxX, maxY) = orderedPair $ snd box
    in
      unlines $ fmap (showLine markers minX maxX) [minY..maxY]
  where orderedPair = (x_coord &&& y_coord)
        showLine markers minX maxX y = fmap (marker markers y) [minX..maxX]
        marker markers y x = if Set.member (x, y) markers then '#' else '.'


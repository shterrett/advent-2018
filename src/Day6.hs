{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Day6 where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as Set
import Data.HashSet (HashSet)
import Data.Maybe (mapMaybe)
import Text.Parsec
import Text.Parser.Token (integer)
import Data.List (maximumBy, minimumBy)
import Control.Arrow ((&&&))

data Point = Point { x_coord :: Integer, y_coord :: Integer }
           deriving (Show, Eq, Generic, Hashable)

data Owner = Single Integer Point
           | Tie Integer [Point]
           deriving (Show, Eq)

type RefPoints = [Point]
type Field = [Point]
type Border = [Point]

parseInput :: [T.Text] -> Either ParseError RefPoints
parseInput = sequence . fmap ((parse pointParser "") . T.unpack)

pointParser :: Parsec String () Point
pointParser = Point <$> integer <*> (string ", " >> integer)

boundingBox :: RefPoints -> (Point, Point)
boundingBox ps = (,) (corner minCoord ps) (corner maxCoord ps)
    where maxCoord = maximumBy compare
          minCoord = minimumBy compare
          corner :: ([Integer] -> Integer) -> [Point] -> Point
          corner bound ps = (uncurry Point) $ (bound . fmap x_coord) &&& (bound . fmap y_coord) $ ps

pointsInBox :: (Point, Point) -> Field
pointsInBox ((Point xmin ymin), (Point xmax ymax)) =
    Point <$> [xmin..xmax] <*> [ymin..ymax]

closestPoints :: Field -> RefPoints -> HashMap Point Owner
closestPoints box ref = foldr (claimPoints ref) Map.empty box

claimPoints :: RefPoints -> Point -> HashMap Point Owner -> HashMap Point Owner
claimPoints ref p ps = Map.insert p (closestPoint ref p) ps

closestPoint :: RefPoints -> Point -> Owner
closestPoint (r:rs) p = foldr (cmpPoint p) (Single (dist r p) r) rs
  where cmpPoint :: Point -> Point -> Owner -> Owner
        cmpPoint p refp o@(Single d' refp') =
          let
            d = dist p refp
          in
            case compare d d' of
              LT -> Single d refp
              EQ -> Tie d [refp, refp']
              GT -> o
        cmpPoint p refp o@(Tie d' ps) =
          let
            d = dist p refp
          in
            case compare d d' of
              LT -> Single d refp
              EQ -> Tie d (refp:ps)
              GT -> o
        dist :: Point -> Point -> Integer
        dist (Point x1 y1) (Point x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

borderPoints :: (Point, Point) -> Field -> Border
borderPoints ((Point xmin ymin), (Point xmax ymax)) =
    filter (\(Point x y) ->
            x == xmin ||
            x == xmax ||
            y == ymin ||
            y == ymax)

infinitePoints :: HashMap Point Owner -> RefPoints -> HashSet Point
infinitePoints ps = Set.fromList .
                    mapMaybe (infinitePoint ps)

infinitePoint :: HashMap Point Owner -> Point -> Maybe Point
infinitePoint ps p =
    case Map.lookup p ps of
      Nothing -> Nothing
      Just (Tie _ _) -> Nothing
      Just (Single _ p') -> Just p'

greatestArea :: RefPoints -> Integer
greatestArea ref =
    let
      bbox = boundingBox ref
      boxPoints = pointsInBox bbox
      claimed = closestPoints boxPoints ref
      infinite = infinitePoints claimed $ borderPoints bbox boxPoints
      finite = Set.difference (Set.fromList ref) infinite
    in
      maximumBy compare $
      Map.elems $
      foldr (accumArea finite) Map.empty (Map.toList claimed)
    where
      accumArea :: HashSet Point ->
                   (Point, Owner) ->
                   HashMap Point Integer ->
                   HashMap Point Integer
      accumArea fs (_, (Single _ p)) ps =
        if Set.member p fs then
          Map.insertWith (+) p 1 ps
        else
          ps
      accumArea _ _ ps = ps

day6 :: [T.Text] -> T.Text
day6 lines = either (T.pack . show) (T.pack . show) $
             greatestArea <$>
             parseInput lines

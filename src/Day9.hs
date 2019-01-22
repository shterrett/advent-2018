{-# LANGUAGE OverloadedStrings #-}

module Day9 where

import qualified Data.Text as T
import Data.Sequence (Seq( (:<|) ), (><), (<|))
import qualified Data.Sequence as Seq
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Tuple (swap)
import Data.List (foldl', maximumBy)
import Control.Arrow ((&&&), (***))
import Text.Parsec
import Text.Parser.Token (integer)

data Circle = Circle { marbles :: (Seq Integer)
                     , circumference :: Int
                     }
                 deriving (Show, Eq)

data Game = Game { circle :: Circle
                 , scores :: HashMap Elf Integer
                 }
                 deriving (Show, Eq)

type Marble = Integer
type Elf = Integer

nextPos :: Int
nextPos = 2

scoringMarble :: Marble -> Bool
scoringMarble m = m > 0 && m `mod` 23 == 0

scoreJump :: Int
scoreJump = 7

scorePos :: Int -> Int
scorePos l | l < scoreJump = l - (scoreJump `mod` l)
           | otherwise = l - scoreJump

fromList :: [Integer] -> Circle
fromList = uncurry Circle . (Seq.fromList &&& length)

insertNew :: Integer -> Circle -> Circle
insertNew i (Circle s l) = Circle (insert i s) (l + 1)
  where insert :: Integer -> Seq Integer -> Seq Integer
        insert i = uncurry (><) . swap . fmap (i <|) . Seq.splitAt nextPos

score :: Circle -> (Integer, Circle)
score (Circle seq len) =
    let
      (t, (s:<|h)) = Seq.splitAt (scorePos len) seq
    in
      (s, Circle (h >< t) (len - 1))

turns :: (Elf, Marble) -> [(Elf, Marble)]
turns = uncurry zip . (cycle . (range 1) *** (range 0))
  where range x y = [x..y]

takeTurn :: Game -> (Elf, Marble) -> Game
takeTurn (Game c s) (e, m)
  | scoringMarble m =
    let
      (v, c') = score c
    in
      Game c' $ Map.insertWith (+) e (m + v) s
  | otherwise = Game (insertNew m c) s

playGame :: [(Elf, Marble)] -> Game
playGame = foldl' takeTurn initGame

initGame :: Game
initGame = Game (fromList []) Map.empty

highScore :: Game -> Integer
highScore = maximumBy compare . Map.elems . scores

parseInput :: [T.Text] -> Either ParseError (Elf, Marble)
parseInput lines =  parse getNumbers "" $ (T.unpack . head) lines
  where getNumbers = (,) <$> integer <*> (string "players; last marble is worth " >> integer)

day9 :: [T.Text] -> T.Text
day9 input = either (T.pack . show) (T.pack . show) $
             highScore . playGame . turns <$>
             parseInput input

day9p2 :: [T.Text] -> T.Text
day9p2 input = either (T.pack . show) (T.pack . show) $
               highScore . playGame . turns . (fmap (*100)) <$>
               parseInput input

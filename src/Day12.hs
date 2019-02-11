{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.Tuple (swap)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.Char (anyChar, space)

type Pot = Char
type Pots = String
type Window = Pots
type Rules = HashMap String Char
data Game = Game { pots :: Pots
                 , rules :: Rules
                 , potCount :: Int
                 }

parseInput :: [T.Text] -> Either ParseError Game
parseInput (state:empty:rules) = buildGame (parsePots $ T.unpack state) <$>
                                (Map.fromList <$>
                                 (sequence $ (parse ruleParser "" . T.unpack) <$> rules))

buildGame :: Pots -> Rules -> Game
buildGame ps rs = Game (extendPots ps) rs (length ps)

parsePots :: String -> Pots
parsePots = drop 15

ruleParser :: Parsec String () (String, Char)
ruleParser = (,) <$> manyTill pot space <*> (string "=> " >> pot)
  where pot = oneOf ".#"

windows :: Int -> [a] -> [[a]]
windows n ls | length ls < n = []
             | otherwise = (take n ls):(windows n $ drop 1 ls)

stepPot :: Rules -> Window -> Pot
stepPot rules = fromMaybe '.' . (flip Map.lookup) rules

nextGen :: Rules -> [Window] -> Pots
nextGen rules ws = fmap (stepPot rules) ws

augmentPots :: Int -> Pots -> Pots
augmentPots n ps =
    let extra = take n $ cycle "."
    in extra ++ ps ++ extra

extendPots :: Pots -> Pots
extendPots ps =
    let
      plantsInBeginning = hasPlants $ firstFive ps
      plantsInEnd = hasPlants (firstFive $ reverse ps)
    in
      if plantsInBeginning || plantsInEnd then augmentPots 5 ps  else ps
  where firstFive = take 5
        hasPlants = any ((==) '#')

simulate :: Int -> Game -> Game
simulate 0 g = g
simulate n g =
    let
      newPots = extendPots $
                nextGen (rules g) (windows 5 . augmentPots 2 . pots $ g)
      nextGame = g { pots = newPots }
    in
      simulate (n - 1) nextGame

sumPlantBearing :: Game -> Int
sumPlantBearing (Game ps _ l) =
    let
      first = -1 * div (length ps - l) 2
    in
      sum $
      fmap snd $
      filter hasPlant $
      zip ps [first, (first + 1)..]
  where hasPlant ('#', _) = True
        hasPlant _ = False
        inBase (_, x) = 0 <= x && x <= 25

day12 :: [T.Text] -> T.Text
day12 input =  either (T.pack . show) (T.pack . show) $
               sumPlantBearing .
               simulate 200 <$>
               parseInput input

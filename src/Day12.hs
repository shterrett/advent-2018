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

parseInput :: [T.Text] -> Either ParseError (Rules, Pots)
parseInput (state:empty:rules) = swap . (parsePots $ T.unpack state,) <$>
                                (Map.fromList <$>
                                 (sequence $ (parse ruleParser "" . T.unpack) <$> rules))

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

augmentPots :: Pots -> Pots
augmentPots ps = ".." ++ ps ++ ".."

simulate :: Int -> (Rules, Pots) -> Pots
simulate = undefined

sumPlantBearing :: Pots -> Int
sumPlantBearing ps =
    sum $ fmap snd $ filter inBase $ filter hasPlant $ zip ps [-2, -1..]
  where hasPlant ('#', _) = True
        hasPlant _ = False
        inBase (_, x) = 0 <= x && x <= 25

day12 :: [T.Text] -> T.Text
day12 input =  either (T.pack . show) (T.pack . show) $
               sumPlantBearing .
               simulate 20 .
               (fmap augmentPots) <$>
               parseInput input

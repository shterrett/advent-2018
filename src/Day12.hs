{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Data.Tuple (swap)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Text.Parsec
import Text.Parsec.Char (anyChar, space)

type Pots = String
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

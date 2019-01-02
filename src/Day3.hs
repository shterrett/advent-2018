{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.HashMap.Strict (HashMap, empty, insertWith, elems, lookupDefault)
import qualified Data.Text as T
import Text.Parsec
import Text.Parser.Token (integer)

data Claim = Claim { elfId :: Integer
                   , left :: Integer
                   , top :: Integer
                   , width :: Integer
                   , height :: Integer
                   }
             deriving (Show, Eq)

type Point = (Integer, Integer)

day3 :: [T.Text] -> T.Text
day3 xs = either (T.pack . show) (T.pack . show) $
          length .
          filter (> 1) .
          elems .
          overlapCount .
          concat .
          (fmap points) <$>
          parseInput xs

day3p2 :: [T.Text] -> T.Text
day3p2 xs = either (T.pack . show) (T.pack . show) $ do
  parsed <- parseInput xs
  let claimCounts = overlapCount . concat . (fmap points) $ parsed
  return $ elfId . head $ filter (unclaimed claimCounts) parsed

parseInput :: [T.Text] -> Either ParseError [Claim]
parseInput = sequence . fmap ((parse lineparser "") . T.unpack)

lineparser :: Parsec String () Claim
lineparser = Claim <$> (char '#' >> integer) <*>
                       (spaces >> char '@' >> spaces >> integer) <*>
                       (char ',' >> integer) <*>
                       (char ':' >> spaces >> integer) <*>
                       (char 'x' >> integer)

points :: Claim -> [Point]
points claim = (,) <$> columns <*> rows
  where columns = [(left claim)..(left claim + width claim - 1)]
        rows = [(top claim)..(top claim + height claim - 1)]

overlapCount :: [Point] -> HashMap Point Integer
overlapCount ps = foldr countClaims empty ps
  where countClaims p m = insertWith (+) p 1 m

unclaimed :: HashMap Point Integer -> Claim -> Bool
unclaimed m c = not $ any (\p -> lookupDefault 1 p m > 1) (points c)

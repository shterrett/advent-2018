{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.HashSet
import qualified Data.Text as T
import Text.Parsec
import Text.Parser.Token (integer)

day1 :: [T.Text] -> T.Text
day1 xs = either (T.pack . show) (T.pack . show . sum) $ parseInput xs

toInt :: T.Text -> Integer
toInt s = case T.unpack s of
            '+':ss -> read ss
            ss -> read ss

day1p2 :: [T.Text] -> T.Text
day1p2 xs = either (T.pack . show) (T.pack . show) $
  (firstRepeat . (scanl (+) 0) . cycle) <$> (parseInput xs)

firstRepeat :: [Integer] -> Integer
firstRepeat xs = firstRepeat' empty xs
  where firstRepeat' :: HashSet Integer -> [Integer] -> Integer
        firstRepeat' _ [] = 0
        firstRepeat' set (x:xs) = if member x set
                                        then x
                                        else firstRepeat' (insert x set) xs

parseInput :: [T.Text] -> Either ParseError [Integer]
parseInput xs = sequence $ ((parse integer "") . T.unpack) <$> xs

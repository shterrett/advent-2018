{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import Data.HashMap.Strict (HashMap, empty, insertWith, elems)
import Control.Arrow ((&&&), (***))

day2 :: [T.Text] -> T.Text
day2 words = (T.pack . show) $
             mult $
             (twoLetters &&& threeLetters) (letterCount <$> words)
  where mult = uncurry (*)
        twoLetters = length . filter (hasCount 2)
        threeLetters = length . filter (hasCount 3)

letterCount :: T.Text -> HashMap Char Integer
letterCount s = foldr incChar empty $ T.unpack s
  where incChar :: Char -> HashMap Char Integer -> HashMap Char Integer
        incChar c m = insertWith (+) c 1 m

hasCount :: Integer -> HashMap Char Integer -> Bool
hasCount i m = any (== i) (elems m)

day2p2 :: [T.Text] -> T.Text
day2p2 words = sameLetters . head $ filter singleDifference allPairs
  where allPairs :: [(T.Text, T.Text)]
        allPairs = (,) <$> words <*> words
        sameLetters :: (T.Text, T.Text) -> T.Text
        sameLetters ws = T.pack $ fmap fst $ filter (uncurry (==)) (charPairs ws)

singleDifference :: (T.Text, T.Text) -> Bool
singleDifference ws = (sum $ delta <$> charPairs ws) == 1
  where delta :: (Char, Char) -> Integer
        delta (a, b) = if a == b then 0 else 1

charPairs :: (T.Text, T.Text) -> [(Char, Char)]
charPairs = (uncurry T.zip)

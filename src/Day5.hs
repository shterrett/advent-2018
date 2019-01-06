{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import qualified Data.Text as T
import Data.Char (isUpper, toLower)
import Data.HashSet (toList, fromList)
import Data.List (minimumBy)

data Monomer = Pos Char
             | Neg Char
             deriving (Show, Eq)

fromChar :: Char -> Monomer
fromChar c = if isUpper c then Neg (toLower c) else Pos c

type Polymer = [Monomer]

type Element = Char

ofElement :: Element -> Monomer -> Bool
ofElement e (Pos x) = e == x
ofElement e (Neg x) = e == x

condense :: Polymer -> Polymer
condense = foldr lengthen []

lengthen :: Monomer -> Polymer -> Polymer
lengthen m@(Pos x) ms@((Neg y):ms') | x == y = ms'
                                    | otherwise = m:ms
lengthen m@(Neg x) ms@((Pos y):ms') | x == y = ms'
                                    | otherwise = m:ms
lengthen m ms = m:ms

day5 :: T.Text -> T.Text
day5 = T.pack . show . length . condense . (fmap fromChar) . T.unpack

removeElement :: Polymer -> Element -> Polymer
removeElement p e = condense $ filter (not . ofElement e) p

shortestPolymer :: Polymer -> Polymer -> Ordering
shortestPolymer x y = compare (length x) (length y)

day5p2 :: T.Text -> T.Text
day5p2 p = let
            es = toList $ fromList (T.unpack p) :: [Element]
            p' = fmap fromChar (T.unpack p)
           in
            T.pack . show . length $
              minimumBy shortestPolymer $ fmap (removeElement p') es

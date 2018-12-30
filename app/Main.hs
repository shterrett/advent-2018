module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day1

main :: IO ()
main = day1p2 <$> (T.lines <$> Tio.readFile "inputs/day-1") >>= Tio.putStrLn

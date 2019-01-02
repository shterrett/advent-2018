module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day3

main :: IO ()
main = day3p2 <$> (T.lines <$> Tio.readFile "inputs/day-3") >>= Tio.putStrLn

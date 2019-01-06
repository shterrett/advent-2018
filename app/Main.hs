module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day5

main :: IO ()
main = day5p2 . head <$> (T.lines <$> Tio.readFile "inputs/day-5") >>= Tio.putStrLn

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day11

main :: IO ()
-- main = day6p2 <$> T.lines <$> Tio.readFile "inputs/day-6" >>= Tio.putStrLn
main = putStrLn . show $ day11p2 18

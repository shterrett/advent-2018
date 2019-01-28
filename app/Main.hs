module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day10

main :: IO ()
main = day10 <$> T.lines <$> Tio.readFile "inputs/day-10" >>= Tio.putStrLn

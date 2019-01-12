module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day6

main :: IO ()
main = day6 <$> T.lines <$> Tio.readFile "inputs/day-6" >>= Tio.putStrLn

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day12

main :: IO ()
main = day12 <$> T.lines <$> Tio.readFile "inputs/day-12" >>= Tio.putStrLn

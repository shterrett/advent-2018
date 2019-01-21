module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day8

main :: IO ()
main = day8p2 <$> T.lines <$> Tio.readFile "inputs/day-8" >>= Tio.putStrLn

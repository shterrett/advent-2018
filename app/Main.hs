module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day4

main :: IO ()
main = day4p2 <$> (T.lines <$> Tio.readFile "inputs/day-4") >>= Tio.putStrLn

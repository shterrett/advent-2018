module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day2

main :: IO ()
main = day2p2 <$> (T.lines <$> Tio.readFile "inputs/day-2") >>= Tio.putStrLn

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day13

main :: IO ()
main = day13 <$> T.lines <$> Tio.readFile "inputs/day-13" >>= Tio.putStrLn

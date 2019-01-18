module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day7

main :: IO ()
main = day7 <$> T.lines <$> Tio.readFile "inputs/day-7" >>= Tio.putStrLn

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day15

main :: IO ()
main = day15 <$> T.lines <$> Tio.readFile "inputs/day-15" >>= Tio.putStrLn

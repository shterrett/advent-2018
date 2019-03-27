{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day16

main :: IO ()
main = day16 <$> T.lines <$> Tio.readFile "inputs/day-16-p1" >>= Tio.putStrLn

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as Tio
import qualified Data.Text as T
import Day16

main :: IO ()
-- main = day16 <$> T.lines <$> Tio.readFile "inputs/day-16-p1" >>= Tio.putStrLn
main = do
    events <- T.lines <$> Tio.readFile "inputs/day-16-p1"
    program <- T.lines <$> Tio.readFile "inputs/day-16-p2"
    Tio.putStrLn $
      either (T.pack . show ) (T.pack . show) $
      day16p2 events >>= day16p2' program

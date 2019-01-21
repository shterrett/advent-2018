{-# LANGUAGE OverloadedStrings #-}

module Day8Spec where

import Test.Hspec
import Day8

spec :: Spec
spec = do
    describe "parsing input" $ do
      it "returns a list of numbers" $ do
        let input = ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
        parseInput input `shouldBe` 
          (Right [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2])
    describe "extracting metadata" $ do
      it "extracts the metadata from the list of nodes" $ do
        let nodes = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
        extractMetadata nodes `shouldBe` [10, 11, 12, 99, 2, 1, 1, 2]
    describe "day8" $ do
      it "sums the values of the metadata entries" $ do
        let input = ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
        day8 input `shouldBe` "138"

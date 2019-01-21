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
      it "extracts the metadata from the data" $ do
        let tree = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
        extractMetadata (fst $ buildTree tree) `shouldBe` [1, 1, 2, 10, 11, 12, 2, 99]
    describe "day8" $ do
      it "sums the values of the metadata entries" $ do
        let input = ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
        day8 input `shouldBe` "138"
    describe "buildTree" $ do
      it "builds an explicit tree out of the given representation" $ do
        let tree = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
        let nodeD = Node [] [99]
        let nodeC = Node [nodeD] [2]
        let nodeB = Node [] [10, 11, 12]
        let nodeA = Node [nodeB, nodeC] [1, 1, 2]
        buildTree tree `shouldBe` (nodeA, [])
    describe "value of a node" $ do
      it "returns the sum of the metadata when the node has no children" $ do
        let node = Node [] [1, 3, 5]
        nodeValue node `shouldBe` (sum [1, 3, 5])
      it "returns the sum of the value of the child nodes 1-indexed by the metadata" $ do
        let nodeC = Node [] [2]
        let nodeB = Node [] [10, 11, 12]
        let nodeA = Node [nodeB, nodeC] [1, 1, 2]
        nodeValue nodeA `shouldBe` (sum [10, 11, 12]) * 2 + (sum [2])
      it "ignores nodes that are not indexed" $ do
        let nodeC = Node [] [2]
        let nodeB = Node [] [10, 11, 12]
        let nodeA = Node [nodeB, nodeC] [1]
        nodeValue nodeA `shouldBe` (sum [10, 11, 12])
      it "counts indexes that don't exist as 0" $ do
        let nodeC = Node [] [2]
        let nodeB = Node [] [10, 11, 12]
        let nodeA = Node [nodeB, nodeC] [1, 3]
        nodeValue nodeA `shouldBe` (sum [10, 11, 12])
    describe "day8p2" $ do
      it "returns the value of the root node of the input tree" $ do
        let input = ["2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
        day8p2 input `shouldBe` "66"

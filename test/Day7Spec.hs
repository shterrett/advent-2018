{-# LANGUAGE OverloadedStrings #-}

module Day7Spec where

import Test.Hspec
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Day7

spec :: Spec
spec = do
    describe "parsing input" $ do
      it "parses input into edges" $ do
        let input = [ "Step C must be finished before step A can begin."
                    , "Step C must be finished before step F can begin."
                    , "Step A must be finished before step B can begin."
                    , "Step A must be finished before step D can begin."
                    , "Step B must be finished before step E can begin."
                    , "Step D must be finished before step E can begin."
                    , "Step F must be finished before step E can begin."
                    ]
        parseInput input `shouldBe`
          (Right [ ('C', 'A')
                 , ('C', 'F')
                 , ('A', 'B')
                 , ('A', 'D')
                 , ('B', 'E')
                 , ('D', 'E')
                 , ('F', 'E')
                 ])
    describe "building Graaph" $ do
      it "creates an adjacency map of nodes to all incident nodes" $ do
        let edges = [ ('C', 'A')
                    , ('C', 'F')
                    , ('A', 'B')
                    , ('A', 'D')
                    , ('B', 'E')
                    , ('D', 'E')
                    , ('F', 'E')
                    ]
        buildGraph edges `shouldBe`
          Map.fromList [ ('C', ['A', 'F'])
                       , ('A', ['B', 'D'])
                       , ('B', ['E'])
                       , ('D', ['E'])
                       , ('F', ['E'])
                       ]
    describe "building edge count" $ do
      it "creates a map from a node to the count of its incoming edges" $ do
        let edges = [ ('C', 'A')
                    , ('C', 'F')
                    , ('A', 'B')
                    , ('A', 'D')
                    , ('B', 'E')
                    , ('D', 'E')
                    , ('F', 'E')
                    ]
        buildEdgeCount edges `shouldBe`
          Map.fromList [ ('A', 1)
                       , ('F', 1)
                       , ('B', 1)
                       , ('D', 1)
                       , ('E', 3)
                       ]
    describe "sources" $ do
      it "produces a list of nodes that have no incoming edges" $ do
        let edges = [ ('C', 'A')
                    , ('C', 'F')
                    , ('A', 'B')
                    , ('A', 'D')
                    , ('B', 'E')
                    , ('D', 'E')
                    , ('F', 'E')
                    ]
        findSources (buildGraph edges) (buildEdgeCount edges) `shouldBe`
          ['C']
    -- Modifications to the ordering of a node make the topological sort
    -- fail for part 2
    describe "topographical sort" $ do
      it "produces a valid topographical ordering of the graph" $ do
        let edges = [ ('C', 'A')
                    , ('C', 'F')
                    , ('A', 'B')
                    , ('A', 'D')
                    , ('B', 'E')
                    , ('D', 'E')
                    , ('F', 'E')
                    ]
        topologicalSort (initState 1 (const 0) edges) `shouldBe` "CABDFE"
    describe "day7" $ do
      it "returns the topological sort of the graph" $ do
        let input = [ "Step C must be finished before step A can begin."
                    , "Step C must be finished before step F can begin."
                    , "Step A must be finished before step B can begin."
                    , "Step A must be finished before step D can begin."
                    , "Step B must be finished before step E can begin."
                    , "Step D must be finished before step E can begin."
                    , "Step F must be finished before step E can begin."
                    ]
        day7 input `shouldBe` "CABDFE"
    describe "day7p2" $ do
      it "returns the total time it takes multiple workers to complete the steps" $ do
        let input = [ "Step C must be finished before step A can begin."
                    , "Step C must be finished before step F can begin."
                    , "Step A must be finished before step B can begin."
                    , "Step A must be finished before step D can begin."
                    , "Step B must be finished before step E can begin."
                    , "Step D must be finished before step E can begin."
                    , "Step F must be finished before step E can begin."
                    ]
        day7p2 2 0 input `shouldBe` "15"

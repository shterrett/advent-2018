{-# LANGUAGE TupleSections #-}

module Day15DijkstraSpec where

import Test.Hspec
import qualified Data.HashMap.Strict as Map
import qualified Data.Heap as Heap
import Day15

spec :: Spec
spec = do
    describe "finding shortest path" $ do
      it "finds the shortest path to a given square" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (3, 3)
        let finish = (2, 2)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [(2, 3), (2, 2)]
      it "doesn't move through elves or goblins" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##G..##"
                      , "#######"
                      ]
        let start = (3, 3)
        let finish = (2, 2)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (4, 3)
                                                     , (4, 2)
                                                     , (4, 1)
                                                     , (3, 1)
                                                     , (2, 1)
                                                     , (2, 2)
                                                     ]
      it "prefers moving to the left if there are two options to the same square" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (3, 3)
        let finish = (3, 1)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (2, 3)
                                                     , (2, 2)
                                                     , (2, 1)
                                                     , (3, 1)
                                                     ]
      it "prefers moving up if there are two options to the same square" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (2, 2)
        let finish = (4, 2)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (2, 1)
                                                     , (3, 1)
                                                     , (4, 1)
                                                     , (4, 2)
                                                     ]
      it "prefers moving up over left if they are both options to the same point" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##...##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (3, 3)
        let finish = (2, 1)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (3, 2)
                                                     , (3, 1)
                                                     , (2, 1)
                                                     ]
      it "prefers moving right over down if they are both options to the same point" $ do
        let caveMap = [ "#######"
                      , "#.....#"
                      , "#.#...#"
                      , "#.....#"
                      , "#######"
                      ]
        let start = (1, 1)
        let finish = (4, 3)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (2, 1)
                                                     , (3, 1)
                                                     , (4, 1)
                                                     , (4, 2)
                                                     , (4, 3)
                                                     ]
      it "complex example for moving right over down" $ do
        let caveMap = [ "########"
                      , "###G...#"
                      , "###.G..#"
                      , "##.....#"
                      , "##G...##"
                      , "##..G.##"
                      , "#..GG.##"
                      , "#.....##"
                      , "#.....##"
                      , "#...E..#"
                      , "########"
                      ]
        let start = (3, 1)
        let finish = (4, 8)
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        path (predecessors result) finish `shouldBe` [ (4, 1)
                                                     , (5, 1)
                                                     , (5, 2)
                                                     , (5, 3)
                                                     , (5, 4)
                                                     , (5, 5)
                                                     , (5, 6)
                                                     , (5, 7)
                                                     , (4, 7)
                                                     , (4, 8)
                                                     ]
    describe "finding nearest point" $ do
      it "returns the nearest point in the list of destinations" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (3, 3)
        let dests = [(2, 2), (4, 1)]
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        nearestPoint dests result `shouldBe` Just (2, 2)
      it "prefers the first point in reading order if multiple points have the same cost" $ do
        let caveMap = [ "#######"
                      , "##...##"
                      , "##.#.##"
                      , "##...##"
                      , "#######"
                      ]
        let start = (3, 3)
        let dests = [(4, 1), (3, 1), (2, 1)]
        let result = dijkstra $ initDijkstra start (initGame caveMap)
        nearestPoint dests result `shouldBe` Just (2, 1)

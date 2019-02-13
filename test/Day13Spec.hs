{-# LANGUAGE OverloadedStrings #-}

module Day13Spec where

import Test.Hspec
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Heap as Heap
import Day13

spec :: Spec
spec = do
    describe "parsing tracks" $ do
      let input = [ "/->--\\"
                  , "^    |"
                  , "|    v"
                  , "\\--<-/"
                  ]

      it "creates a map of each track segment's position and class" $ do
        segments (parseInput input) `shouldBe`
          Map.fromList [ ((0, 0), NTurn)
                       , ((1, 0), Horizontal)
                       , ((2, 0), Horizontal)
                       , ((3, 0), Horizontal)
                       , ((4, 0), Horizontal)
                       , ((5, 0), PTurn)
                       , ((0, 1), Vertical)
                       , ((5, 1), Vertical)
                       , ((0, 2), Vertical)
                       , ((5, 2), Vertical)
                       , ((0, 3), PTurn)
                       , ((1, 3), Horizontal)
                       , ((2, 3), Horizontal)
                       , ((3, 3), Horizontal)
                       , ((4, 3), Horizontal)
                       , ((5, 3), NTurn)
                       ]
      it "creates a set of the current positions of each car" $ do
        occupied (parseInput input) `shouldBe`
          Set.fromList [ (2, 0)
                       , (0, 1)
                       , (5, 2)
                       , (3, 3)
                       ]
      it "creates a heap of cars" $ do
        cars (parseInput input) `shouldBe`
          (Heap.fromAscList [ Car (2, 0) PosX LeftTurn
                            , Car (0, 1) NegY LeftTurn
                            , Car (5, 2) PosY LeftTurn
                            , Car (3, 3) NegX LeftTurn
                            ] :: Heap.MinHeap Car)
    describe "updateCar" $ do
      it "continues in the direction of travel on a straightaway" $ do
        let car = Car (3, 5) PosX LeftTurn
        let ps = Map.fromList [((3, 5), Horizontal)]
        updateCar ps car `shouldBe`
          (Car (4, 5) PosX LeftTurn)
        let car' = Car (3, 5) NegY LeftTurn
        let ps' = Map.fromList [((3, 5), Vertical)]
        updateCar ps' car' `shouldBe`
          (Car (3, 4) NegY LeftTurn)
      it "makes a negative turn" $ do
        let car = Car (3, 5) PosX LeftTurn
        let ps = Map.fromList [((3, 5), NTurn)]
        updateCar ps car `shouldBe`
          (Car (3, 4) NegY LeftTurn)
      it "makes a positive turn" $ do
        let car = Car (3, 5) NegY LeftTurn
        let ps = Map.fromList [((3, 5), PTurn)]
        updateCar ps car `shouldBe`
          (Car (2, 5) NegX LeftTurn)
      it "turns left after turning right at a previous intersection" $ do
        let car = Car (3, 5) PosX LeftTurn
        let ps = Map.fromList [((3, 5), Intersection)]
        updateCar ps car `shouldBe`
          (Car (3, 4) NegY Straight)
      it "goes straight after turning left at a previous intersection" $ do
        let car = Car (3, 5) NegY Straight
        let ps = Map.fromList [((3, 5), Intersection)]
        updateCar ps car `shouldBe`
          (Car (3, 4) NegY RightTurn)
      it "turns right after going straight at a previous intersection" $ do
        let car = Car (3, 5) NegX RightTurn
        let ps = Map.fromList [((3, 5), Intersection)]
        updateCar ps car `shouldBe`
          (Car (3, 4) NegY LeftTurn)
    describe "moveCar" $ do
      it "returns Left Point when a collision occurs" $ do
        let car = Car (3, 5) NegX Straight
        let ps = Map.fromList [((3, 5), Horizontal)]
        let tracks = Tracks ps (Set.fromList [(2, 5), (3, 5)]) Heap.empty
        moveCar tracks car `shouldBe` Left (2, 5)
      it "returns an updated tracks with the new car" $ do
        let car = Car (3, 5) NegX Straight
        let ps = Map.fromList [((3, 5), Horizontal)]
        let tracks = Tracks ps (Set.fromList [(1, 1), (3, 5)]) Heap.empty
        moveCar tracks car `shouldBe` (
          Right $ Tracks ps
                          (Set.fromList [(1, 1), (2, 5)])
                          (Heap.fromList [Car (2, 5) NegX Straight]))
    describe "day13" $ do
      it "returns the point of the first collision" $ do
        let input = [ "/->-\\        "
                    , "|   |  /----\\"
                    , "| /-+--+-\\  |"
                    , "| | |  | v  |"
                    , "\\-+-/  \\-+--/"
                    , "  \\------/   "
                    ]
        day13 input `shouldBe` "(7,3)"
    describe "day13p2" $ do
      it "returns the point that the last suriving car reaches" $ do
        let input = [ "/>-<\\  "
                    , "|   |  "
                    , "| /<+-\\"
                    , "| | | v"
                    , "\\>+</ |"
                    , "  |   ^"
                    , "  \\<->/"
                    ]
        day13p2 input `shouldBe` "(6,4)"

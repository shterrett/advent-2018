{-# LANGUAGE OverloadedStrings #-}

module Day12Spec where

import Test.Hspec
import qualified Data.HashMap.Strict as Map
import Data.HashMap.Strict (HashMap)
import Day12

spec :: Spec
spec = do
    describe "parsing input" $ do
      it "parses the list of pots into a list of chars and potentially extends" $ do
        let input = ["initial state: #..#.#..##......###...###"
                    , ""
                    , "...## => #"
                    , "..#.. => #"
                    , ".#... => #"
                    , ".#.#. => #"
                    ]
        pots <$> parseInput input `shouldBe` (Right ".....#..#.#..##......###...###.....")
    describe "windows" $ do
      it "returns a list of windows of the given length with step size 1" $ do
        let l = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
        windows 3 l `shouldBe` [ [1, 2, 3]
                               , [2, 3, 4]
                               , [3, 4, 5]
                               , [4, 5, 6]
                               , [5, 6, 7]
                               , [6, 7, 8]
                               , [7, 8, 9]
                               , [8, 9, 10]
                               ]
    describe "next generation" $ do
      let rules = Map.insert "...##" '#' $
                  Map.insert "..#.." '#' $
                  Map.insert ".#..." '#' $
                  Map.insert ".#.#." '#' $
                  Map.empty

      it "acts on a single list according to the parameters" $ do
        let l1 = "..#.."
        let l2 = "#...#"
        stepPot rules l1 `shouldBe` '#'
        stepPot rules l2 `shouldBe` '.'
      it "maps over a list of windows" $ do
        nextGen rules [ "..#.."
                      , "#...#"
                      , ".#..."
                      , "....."
                      , ".#.#."
                      ]
          `shouldBe` "#.#.#"
    describe "augment pots" $ do
      it "adds two pots to the beginning and two to the end so the given pots are always considered" $ do
        augmentPots 2 "#......#" `shouldBe` "..#......#.."
    describe "sumPlantBearing" $ do
      it "returns the sum of the index of the plant-bearing pots, remembering to account for the extra 2 on each end" $ do
        let game = Game ".#....##....#####...#######....#.#..##." 
                        Map.empty
                        32
        sumPlantBearing game `shouldBe` 325
    describe "simulate" $ do
      it "carries out the simulation a given number of times" $ do
        let pots = "#..#.#..##......###...###"
        let rules = Map.fromList [ ("...##", '#')
                                 , ("..#..", '#')
                                 , (".#...", '#')
                                 , (".#.#.", '#')
                                 , (".#.##", '#')
                                 , (".##..", '#')
                                 , (".####", '#')
                                 , ("#.#.#", '#')
                                 , ("#.###", '#')
                                 , ("##.#.", '#')
                                 , ("##.##", '#')
                                 , ("###..", '#')
                                 , ("###.#", '#')
                                 , ("####.", '#')
                                 ]
        let game = buildGame pots rules
        sumPlantBearing (simulate 20 game) `shouldBe` 325

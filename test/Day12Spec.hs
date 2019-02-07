{-# LANGUAGE OverloadedStrings #-}

module Day12Spec where

import Test.Hspec
import Day12

spec :: Spec
spec = do
    describe "parsing input" $ do
      it "parses the list of pots into a list of chars" $ do
        let input = ["initial state: #..#.#..##......###...###"
                    , ""
                    , "...## => #"
                    , "..#.. => #"
                    , ".#... => #"
                    , ".#.#. => #"
                    ]
        snd <$> parseInput input `shouldBe` (Right "#..#.#..##......###...###")


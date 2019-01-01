{-# LANGUAGE OverloadedStrings #-}

module Day2Spec where

import Test.Hspec
import Data.HashMap.Strict
import Day2

spec :: Spec
spec = do
    describe "letter count" $ do
      it "counts each letter in a string" $ do
        let s = "abcbaca"
        let map = (insert 'a' 3) $
                  (insert 'b' 2) $
                  (insert 'c' 2) $
                  empty
        letterCount s `shouldBe` map
    describe "hasCOunt" $ do
      it "returns true if there is a letter with the given count" $ do
        let map = (insert 'a' 3) $
                  (insert 'b' 2) $
                  (insert 'c' 2) $
                  empty
        hasCount 2 map `shouldBe` True
        hasCount 1 map `shouldBe` False
    describe "day2" $ do
      it "returns the product of words with 2 letters and words with 3 letters" $ do
        let words = [ "abcdef"
                    , "bababc"
                    , "abbcde"
                    , "abcccd"
                    , "aabcdd"
                    , "abcdee"
                    , "ababab"
                    ]
        day2 words `shouldBe` "12"
    describe "singleDifference" $ do
      it "returns true when there is only one letter difference between strings" $ do
        let pairs = [ ("abcde", "axcye")
                    , ("fghij", "fguij")
                    , ("klmno", "klmno")
                    , ("pqrst", "wvxyz")
                    ]
        singleDifference <$> pairs `shouldBe` [False, True, False, False]
    describe "day2p2" $ do
      it "returns the letters that are shared between the two words with exactly one letter different" $ do
        let words = [ "abcde"
                    , "fghij"
                    , "klmno"
                    , "pqrst"
                    , "fguij"
                    , "axcye"
                    , "wvxyz"
                    ]
        day2p2 words `shouldBe` "fgij"

{-# LANGUAGE OverloadedStrings #-}

module Day16Spec where

import Test.Hspec
import Day16Opcode
import Day16
import Text.Parsec (parse)

spec :: Spec
spec = do
    describe "parsing events" $ do
      it "parses an event into two registers plus a call" $ do
        let event = unlines $ [ "Before: [3, 2, 1, 1]"
                              , "9 2 1 2"
                              , "After:  [3, 2, 2, 1]"
                              ]
        parse eventParser "" event `shouldBe` Right (Event (mkRegister 3 2 1 1)
                                                           (mkRegister 3 2 2 1)
                                                           9
                                                           (2, 1, 2))
      it "parses the input into a list of events" $ do
        let input = [ "Before: [1, 0, 2, 1]"
                    , "2 3 2 0"
                    , "After:  [1, 0, 2, 1]"
                    , ""
                    , "Before: [1, 0, 2, 2]"
                    , "11 3 2 1"
                    , "After:  [1, 0, 2, 2]"
                    ]
        parseInput input `shouldBe` Right [ Event (mkRegister 1 0 2 1)
                                                  (mkRegister 1 0 2 1)
                                                  2
                                                  (3, 2, 0)
                                          , Event (mkRegister 1 0 2 2)
                                                  (mkRegister 1 0 2 2)
                                                  11
                                                  (3, 2, 1)
                                          ]
    describe "countOpcodes" $ do
      it "counts the number of opcodes that successfully take the before to after" $ do
        let event = Event (mkRegister 3 2 1 1)
                          (mkRegister 3 2 2 1)
                          9
                          (2, 1, 2)
        countOpcodes event `shouldBe` 3

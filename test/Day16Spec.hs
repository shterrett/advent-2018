{-# LANGUAGE OverloadedStrings #-}

module Day16Spec where

import Test.Hspec
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
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
    describe "successful opcodes" $ do
      it "returns a list of the names of the opcodes that give the proper result" $ do
        let event = Event (mkRegister 3 2 1 1)
                          (mkRegister 3 2 2 1)
                          9
                          (2, 1, 2)
        (name <$> successfulOpcodes event) `shouldBe` ["addi", "mulr", "seti"]
    describe "winnowing opcodes" $ do
      it "finds the common opcode for multiple events with the same operation" $ do
        let events = [ Event (mkRegister 3 2 1 1)
                             (mkRegister 3 2 2 1)
                             9
                             (2, 1, 2)
                     , Event (mkRegister 3 2 1 1)
                             (mkRegister 3 2 4 1)
                             9
                             (4, 1, 2)
                     ]
        (name $
          head $
          Set.toList $
          fromJust $
          Map.lookup 9 $
          winnowOpcodes events
          ) `shouldBe` "seti"
    describe "opcodeMapping" $ do
      it "correctly assigns each opcode number to a particular opcode" $ do
        let winnowed = Map.fromList [ (0, Set.fromList [addr, mulr, muli])
                                    , (1, Set.fromList [muli])
                                    , (2, Set.fromList [addr, muli])
                                    , (3, Set.fromList [addi, mulr])
                                    ]
        let opMap = Map.fromList [ (0, mulr)
                                 , (1, muli)
                                 , (2, addr)
                                 , (3, addi)
                                 ]
        opcodeMapping winnowed `shouldBe` opMap

module Day16OpcodeSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))
import Data.Bits ((.&.), (.|.))
import Data.Maybe (isNothing, isJust, mapMaybe)
import Day16Opcode

inRange x = 0 <= x && x <= 3

instance Arbitrary RegisterIdx where
    arbitrary = elements $ mapMaybe mkRegisterIdx [0, 1, 2, 3]

instance Arbitrary Register where
    arbitrary = mkRegister <$>
                arbitrary <*>
                arbitrary <*>
                arbitrary <*>
                arbitrary

data BinOp = BinOp (Int -> Int -> Int)

instance Arbitrary BinOp where
    arbitrary = elements [ BinOp (+)
                         , BinOp (*)
                         , BinOp (.|.)
                         , BinOp (.&.)
                         ]

instance Show BinOp where
    show _ = "BinOp"

propExecuteReg :: RegisterIdx ->
                  RegisterIdx ->
                  RegisterIdx ->
                  Register ->
                  BinOp ->
                  Bool
propExecuteReg a b c r (BinOp op) =
    let
      result = executeRegister op (getIdx a) (getIdx b) (getIdx c) r
      expected = Just $ put r (op (fetch r a) (fetch r b)) c
    in
      result == expected

propExecuteRegFail :: Int ->
                      Int ->
                      Int ->
                      Register ->
                      BinOp ->
                      Bool
propExecuteRegFail a b c r (BinOp op) =
    let
      result = executeRegister op a b c r
    in
      if all inRange [a, b, c]
        then isJust result
        else isNothing result

propExecuteVal :: Int ->
                  Int ->
                  RegisterIdx ->
                  Register ->
                  BinOp ->
                  Bool
propExecuteVal a b c r (BinOp op) =
    let
      result = executeImmediate op a b (getIdx c) r
      expected = Just $ put r (op a b) c
    in
      result == expected

propExecuteValFail :: Int ->
                      Int ->
                      Int ->
                      Register ->
                      BinOp ->
                      Bool
propExecuteValFail a b c r (BinOp op) =
    let
      result = executeImmediate op a b c r
    in
      if inRange c
        then isJust result
        else isNothing result

propExecuteRI :: RegisterIdx ->
                 Int ->
                 RegisterIdx ->
                 Register ->
                 BinOp ->
                 Bool
propExecuteRI a b c r (BinOp op) =
    let
      result = executeRI op (getIdx a) b (getIdx c) r
      expected = Just $ put r (op (fetch r a) b) c
    in
      result == expected

propExecuteIR :: Int ->
                 RegisterIdx ->
                 RegisterIdx ->
                 Register ->
                 BinOp ->
                 Bool
propExecuteIR a b c r (BinOp op) =
    let
      result = executeIR op a (getIdx b) (getIdx c) r
      expected = Just $ put r (op a (fetch r b)) c
    in
      result == expected

propExecuteRIFail :: Int ->
                     Int ->
                     Int ->
                     Register ->
                     BinOp ->
                     Bool
propExecuteRIFail a b c r (BinOp op) =
    let
      result = executeRI op a b c r
    in
      if all inRange [a, c]
        then isJust result
        else isNothing result

propExecuteIRFail :: Int ->
                     Int ->
                     Int ->
                     Register ->
                     BinOp ->
                     Bool
propExecuteIRFail a b c r (BinOp op) =
    let
      result = executeIR op a b c r
    in
      if all inRange [b, c]
        then isJust result
        else isNothing result

propSetR :: RegisterIdx ->
            Int ->
            RegisterIdx ->
            Register ->
            Bool
propSetR a b c r =
    let
      result = setr (getIdx a) b (getIdx c) r
      expected = Just $ put r (fetch r a) c
    in
      result == expected

propSetI :: Int ->
            Int ->
            RegisterIdx ->
            Register ->
            Bool
propSetI a b c r =
    let
      result = seti a b (getIdx c) r
      expected = Just $ put r a c
    in
      result == expected

propSetRFail :: Int ->
                Int ->
                Int ->
                Register ->
                Bool
propSetRFail a b c r =
    let
      result = setr a b c r
    in
      if all inRange [a, c]
        then isJust result
        else isNothing result

propSetIFail :: Int ->
                Int ->
                Int ->
                Register ->
                Bool
propSetIFail a b c r =
    let
      result = seti a b c r
    in
      if 0 <= c && c <= 3
        then isJust result
        else isNothing result

spec :: Spec
spec = do
    describe "opcodes" $ do
      prop "executes register operations" $ propExecuteReg
      prop "executes value operations" $ propExecuteVal
      prop "fails when any register is out of range" $ propExecuteRegFail
      prop "fails when the destination register is out of range" $ propExecuteValFail
      prop "executes mixed register/value operation" $ propExecuteRI
      prop "executes mixed value/reigster operation" $ propExecuteIR
      prop "fails when a register is out of range" $ propExecuteRIFail
      prop "fails when a register is out of range" $ propExecuteIRFail
      prop "sets register to value of given register" $ propSetR
      prop "sets register to given value" $ propSetI
      prop "setr fails when registers are out of range" $ propSetRFail
      prop "seti fails when target register is out of range" $ propSetIFail

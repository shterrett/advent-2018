module Day16Opcode ( Register
                   , mkRegister
                   , RegisterIdx
                   , mkRegisterIdx
                   , getIdx
                   , OpFn
                   , OpName
                   , OpCode (fn, name)
                   , fetchAt
                   , fetch
                   , putAt
                   , put
                   , executeRegister
                   , executeRI
                   , executeIR
                   , addr
                   , addi
                   , mulr
                   , muli
                   , banr
                   , bani
                   , borr
                   , bori
                   , gtrr
                   , gtri
                   , gtir
                   , eqrr
                   , eqri
                   , eqir
                   , setr
                   , seti
                   , opcodes
                   ) where

import Data.Bits ((.&.), (.|.))
import Data.Hashable (Hashable, hashWithSalt)

data Register = Register Int Int Int Int
              deriving (Show, Eq)

mkRegister :: Int -> Int -> Int -> Int -> Register
mkRegister a b c d = Register a b c d

data RegisterIdx = RegisterIdx { getIdx :: Int }
                 deriving (Show, Eq)

mkRegisterIdx :: Int -> Maybe RegisterIdx
mkRegisterIdx i = if 0 <= i && i <= 3
                    then Just $ RegisterIdx i
                    else Nothing

fetch :: Register -> RegisterIdx -> Int
fetch (Register i _ _ _) (RegisterIdx 0) = i
fetch (Register _ i _ _) (RegisterIdx 1) = i
fetch (Register _ _ i _) (RegisterIdx 2) = i
fetch (Register _ _ _ i) (RegisterIdx 3) = i

put :: Register -> Int -> RegisterIdx -> Register
put (Register a b c d) val (RegisterIdx 0) = Register val b c d
put (Register a b c d) val (RegisterIdx 1) = Register a val c d
put (Register a b c d) val (RegisterIdx 2) = Register a b val d
put (Register a b c d) val (RegisterIdx 3) = Register a b c val

fetchAt :: Register -> Int -> Maybe Int
fetchAt r i = fetch r <$> mkRegisterIdx i

putAt :: Register -> Int -> Int -> Maybe Register
putAt r idx val = put r val <$> mkRegisterIdx idx

type OpFn = Int -> Int -> Int -> Register -> Maybe Register
type OpName = String
data OpCode = OpCode { name  :: OpName
                     , fn :: OpFn
                     }
instance Ord OpCode where
    compare o1 o2 = compare (name o1) (name o2)

instance Hashable OpCode where
    hashWithSalt i o = hashWithSalt i (name o)

instance Eq OpCode where
    (==) o1 o2 = (name o1) == (name o2)

instance Show OpCode where
    show = show . name

executeRegister :: (Int -> Int -> Int) ->
                    Int ->
                    Int ->
                    Int ->
                    Register ->
                    Maybe Register
executeRegister op a b c r = op <$> fetchAt r a <*> fetchAt r b >>= putAt r c

executeRI :: (Int -> Int -> Int) ->
             Int ->
             Int ->
             Int ->
             Register ->
             Maybe Register
executeRI op a b c r = op <$> fetchAt r a <*> return b >>= putAt r c

executeIR :: (Int -> Int -> Int) ->
             Int ->
             Int ->
             Int ->
             Register ->
             Maybe Register
executeIR op a b c r = op <$> return a <*> fetchAt r b >>= putAt r c

addr = OpCode "addr" (executeRegister (+))
addi = OpCode "addi" (executeRI (+))
mulr = OpCode "mulr" (executeRegister (*))
muli = OpCode "muli" (executeRI (*))
borr = OpCode "borr" (executeRegister (.|.))
bori = OpCode "bori" (executeRI (.|.))
banr = OpCode "banr" (executeRegister (.&.))
bani = OpCode "bani" (executeRI (.&.))
gtrr = OpCode "gtrr" (executeRegister gt)
gtri = OpCode "gtri" (executeRI gt)
gtir = OpCode "gtir" (executeIR gt)
eqrr = OpCode "eqrr" (executeRegister eq)
eqri = OpCode "eqri" (executeRI eq)
eqir = OpCode "eqir" (executeIR eq)
setr = OpCode "setr" (\a b c r -> (fetchAt r a) >>= putAt r c)
seti = OpCode "seti" (\a b c r -> putAt r c a)

gt a b = if a > b then 1 else 0
eq a b = if a == b then 1 else 0

opcodes :: [OpCode]
opcodes = [ addr
          , addi
          , mulr
          , muli
          , borr
          , bori
          , banr
          , bani
          , gtrr
          , gtri
          , gtir
          , eqrr
          , eqri
          , eqir
          , setr
          , seti
          ]

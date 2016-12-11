module RegisterMachine where

import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit, digitToInt)
import Data.List (foldl')

doubleAngles :: Integer -> Integer -> Integer
doubleAngles x y = (2 ^ x) * (2 * y + 1)

singleAngles :: Integer -> Integer -> Integer
singleAngles x y = (doubleAngles x y) - 1

data Instruction = PlusReg Integer Integer | MinusReg Integer Integer Integer | HALT
  deriving Show

encodeInstruction :: Instruction -> Integer
encodeInstruction (PlusReg i j)    = doubleAngles (2 * i) j
encodeInstruction (MinusReg i j k) = doubleAngles (2 * i + 1) (singleAngles j k)
encodeInstruction HALT             = 0

encodeList :: [Integer] -> Integer 
encodeList []       = 0
encodeList (x : xs) = doubleAngles x (encodeList xs)

toBinary :: Integer -> String
toBinary x = showIntAtBase 2 intToDigit x ""

fromBinary :: String -> Integer
fromBinary = foldl' (\acc x -> acc * 2 + toInteger(digitToInt  x)) 0

countTrailingChars :: Char -> String -> Integer
countTrailingChars c s = toInteger(length (takeWhile (== c) (reverse s)))

dropFromEnd :: Integer -> String -> String
dropFromEnd x s = (take ((length s) - (fromIntegral x)) s)

decodeBody :: Integer -> Instruction
decodeBody 0 = HALT
decodeBody x = decodeBody' y z
  where
    binary = toBinary x
    y = countTrailingChars '0' binary
    z = fromBinary (dropFromEnd (y + 1) binary)

decodeBody' :: Integer -> Integer -> Instruction
decodeBody' y z = 
  let
    half x = div x 2
    binary = toBinary z
    j      = countTrailingChars '1' binary
    k      = fromBinary (dropFromEnd (j + 1) binary) in
      case mod y 2 of
        0 -> PlusReg (half y) z
        1 -> MinusReg (half (y - 1)) j k

dropLeadingChars :: Char -> String -> (Integer, String)
dropLeadingChars c s = (toInteger(length (takeWhile (== c) s)), dropWhile (== c) s)

decodeIntoList :: String -> [Integer]
decodeIntoList []             = []
decodeIntoList reversedBinary = x : decodeIntoList nextEncoding
  where
    (x, rest)         = dropLeadingChars '0' reversedBinary
    (_, nextEncoding) = dropLeadingChars '1' rest  

decodeProgram :: Integer -> [Instruction]
decodeProgram x = map decodeBody (reverse $ decodeIntoList (toBinary x))

-- Busy beaver
--        contentsOf( r0    ,    r1  )                    newR0     newR1   nextLabel
runBBInstruction :: (Integer, Integer) -> Instruction -> (Integer, Integer, Integer)
runBBInstruction (r0, r1) (PlusReg 0 l) = (r0 + 1, r1, l)
runBBInstruction (r0, r1) (PlusReg 1 l) = (r0, r1 + 1, l)

runBBInstruction (0, r1) (MinusReg 0 success fail) = (0, r1, fail)
runBBInstruction (r0, 0) (MinusReg 1 success fail) = (r0, 0, fail)
runBBInstruction (r0, r1) (MinusReg 0 success fail) = (r0 - 1, r1, success)
runBBInstruction (r0, r1) (MinusReg 1 success fail) = (r0, r1 - 1, success)

-- label -1 used to indicate failure
runBBInstruction (r0, r1) HALT  = (r0, r1, -1)

runBusyBeaver :: [Instruction] -> (Integer, Integer)
runBusyBeaver instructions = runTwoState (0, 0) instructions

runTwoState :: (Integer, Integer) -> [Instruction] -> (Integer, Integer)
runTwoState (r0, r1) instructions = runTwoState' (r0, r1) (head instructions) instructions

runTwoState' :: (Integer, Integer) -> Instruction -> [Instruction] -> (Integer, Integer)
runTwoState' (r0, r1) HALT instructions = (r0, r1)
runTwoState' (r0, r1) currentInstruction instructions = runTwoState' (nextR0, nextR1) nextInstruction instructions
  where
    (nextR0, nextR1, nextLabel) = runBBInstruction (r0, r1) currentInstruction
    nextInstruction = instructionFromLabel nextLabel instructions

instructionFromLabel :: Integer -> [Instruction] -> Instruction
instructionFromLabel (-1) instructions  = HALT
instructionFromLabel label instructions
  | (fromIntegral label) >= (length instructions) = HALT
  | otherwise                      = instructions !! (fromIntegral label)


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

import Data.Char (digitToInt)

sumOfDigits :: (Integral a, Show a) => a -> Int
sumOfDigits = sum . map digitToInt . show

main = print answer
    where answer = sumOfDigits 2 ^ _exp
    	  _exp = 1000

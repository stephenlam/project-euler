divides d n = n `div` d == 0

singles x = words !! (x-1)
  where words = ["one", "two", "three", "four", "five", "six", "seven", "eight",
          "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
          "sixteen", "seventeen", "eighteen", "nineteen"]

tens x = words !! (x-2)
  where words = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy",
                "eighty", "ninety"]

translateBelow100 x
  | x < 20 = singles x
  | divides 10 x = tens x
  | otherwise = tens (div x 10) ++ singles (rem x 10)

translate x = let y = div x 100 in
  

import Data.List (foldl1')
import qualified Data.MemoCombinators as Memo

collatz :: Integer -> Integer
collatz = Memo.arrayRange (1, 999999) collatz'
  where
    collatz' 1 = 1
    collatz' x
      | even x = 1 + collatz' (x`div`2)
      | otherwise = 1 + collatz' (3*x + 1)

maxIndex = snd . foldl1' max . (flip zip [0..])

main = print (maxIndex lengths + 1)
  where lengths = map collatz [1..limit]
        limit = 999999

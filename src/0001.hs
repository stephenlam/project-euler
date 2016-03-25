-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

divides :: (Integral a) => a -> a -> Bool
divides n x = rem x n == 0

main = do
  putStrLn $ "The sum is " ++ show theSum
  where theSum = sum . filter divisible3or5 $ [1..999]
        divisible3or5 x = divides 3 x || divides 5 x

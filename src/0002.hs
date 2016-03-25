-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

fibs :: (Integral a) => [a]
fibs = 0:1:zipWith (+) fibs (tail fibs)

main = do
  putStrLn $ "The sum is " ++ show theSum
  where theSum = sum . filter even . takeWhile (<limit) $ fibs
        limit = 4 * 10^6  -- 4 million

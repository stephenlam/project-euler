sumOfSquares n = n * (n+1) * (2*n + 1) `div` 6
squareOfSum n = (n * (n+1) `div` 2) ^ 2

main = do putStrLn (show diff)
  where diff = squareOfSum n - sumOfSquares n
        n = 100

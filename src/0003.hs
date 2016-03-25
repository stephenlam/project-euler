divides d n = rem n d == 0

ldf k n | divides k n = k
        | k^2 > n = n
        | otherwise = ldf (k+1) n

ld n = ldf 2 n

factors :: (Integral a) => a -> [a]
factors n
  | n < 1 = error "argument not positive"
  | n == 1 = []
  | otherwise = p : factors (div n p)
  where p = ld n

main = do
  putStrLn . show . factors $ 600851475143

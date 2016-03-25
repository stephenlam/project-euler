import Data.List (group)

main = print answer
  where answer = head . dropWhile ((<=500) . divisorCount) $ triangles

--triangles = [n * (n+1) `div` 2 | n <- [1..]]
triangles = do
    n <- [1..]
    return (n * (n+1) `div` 2)
divisorCount = product . map ((1+) . length) . group . factors

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

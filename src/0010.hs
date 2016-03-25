primes = [2,3,5,7] ++ _Y ( (11:)                       -- using a wheel;
                    . minus (scanl (+) 13 $ tail wh11) -- 1.4x faster than
                     . unionAll                        --     the next one
                      . map (\p-> map (p*) . dropWhile (< p) $
                                    scanl (+) (p - rem (p-11) 210) wh11) )

_Y g = g (_Y g)
wh11 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
       4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wh11

unionAll = foldi (\(x:xs) ys-> x:union xs ys) []

-- ordered lists, difference and union
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))

pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

main = do
  putStrLn (show answer)
  where answer = sum (takeWhile (<2000000) primes)

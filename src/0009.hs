searchSpace = [(a,b,n-a-b) | a <- [1..m], b <- [a+1..(n-a)`div`2],
  a < b, a^2 + b^2 == (n-a-b)^2]
  where n = 1000
        m = n `div` 3

main = do
  putStrLn (show searchSpace)

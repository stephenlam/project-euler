import Data.List (sortBy)

combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) (x:xs))) ++ (combinations n xs)

cartesianSquare xs = combinations 2 xs

isPalindrome x = show x == reverse (show x)

palindromeProducts = filter isPalindrome products
  where products = map product . cartesianSquare $ [999,998..100]

main = do
  putStrLn $ show largestPalindromeProduct
    where largestPalindromeProduct = head . sortBy (flip compare) $ palindromeProducts

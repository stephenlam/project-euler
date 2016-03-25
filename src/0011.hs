import System.Environment (getArgs)
import Data.List

interactWith function inputFile = do
    input <- readFile inputFile
    putStrLn . show . function $ input

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [inputFile] -> interactWith function inputFile

          myFunction = largestP

parse :: String -> [[Int]]
parse = (map (map read)) . map words . lines

largest4Prod :: [Int] -> Int
largest4Prod = maximum . go
  where
    go (x:xs)
      | length (x:xs) >= 4 = (product . take 4 $ (x:xs)):(go xs)
      | otherwise = [0]

diagonals :: [[a]] -> [[a]]
diagonals = tail . go [] where
    -- it is critical for some applications that we start producing answers
    -- before inspecting es_
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

-- calculates the largest product in any row, column, or diagonal
largestP input = maximum . map largest4Prod $ rows ++ cols ++ diags
    where rows = parse input
          cols = transpose rows
          diags = diagonals rows ++ diagonals cols

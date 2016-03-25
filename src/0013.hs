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

          myFunction = largeSum

largeSum :: String -> Int
largeSum = read . take 10 . show . sum . map read . map (take sigFigs) . lines
    where sigFigs = 13

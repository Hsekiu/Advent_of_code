import Data.List.Split
import Data.Char
import Debug.Trace

--Since each row has a single set of divisible elements run divisible testing in both directions.
divisible :: [[Int]] -> Int
divisible [x] = divisible' x + divisible' (reverse x)
divisible (x:xs) = (divisible' x + divisible' (reverse x)) + divisible xs

divisible' :: [Int] -> Int
divisible' (x:[]) =  0
divisible' (x:xs) = 
    if length list == 0
        then divisible' xs 
        else if x > head list
            then x `div` head list
            else head list `div` x
    where list = filter (\n -> n `mod` x == 0) xs

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    let result = map (map read) input :: [[Int]]
    print (divisible result)

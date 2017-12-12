import Data.List.Split
import Data.Char

checkSumRow :: [[Int]] -> Int
checkSumRow [x] = checkSumRow' x
checkSumRow (x:xs) = checkSumRow' x + checkSumRow xs

checkSumRow' :: [Int] -> Int
checkSumRow' x = maximum x - minimum x

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    let result = map (map read) input :: [[Int]]
    print (checkSumRow result)
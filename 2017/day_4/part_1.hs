import Data.List
import Data.List.Split

--Takes list of lists and starting val and returns # valid lists.
countValid :: [[String]] -> Int -> Int
countValid  [x] y = 
	--nub returns list without duplicates.
    if length (nub x) == length x then (y + 1) else y
countValid (x:xs) y = 
    if length (nub x) == length x then countValid xs (y + 1)
    else countValid xs y

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    print (countValid input 0)
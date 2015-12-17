import Data.List

convert :: Char -> [Int]
convert x
    | x == '>' = [0,1]
    | x == '<' = [0,-1]
    | x == '^' = [1,0]
    | otherwise = [-1,0]

main = do
    contents <- readFile "input.txt"
    print ((length 
        (nub (scanl1 (zipWith (+)) ([0,0]:(map convert contents))))))
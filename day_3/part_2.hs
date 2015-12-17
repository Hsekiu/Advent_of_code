import Data.List

convert :: Char -> [Int]
convert x
    | x == '>' = [0,1]
    | x == '<' = [0,-1]
    | x == '^' = [1,0]
    | otherwise = [-1,0]

splitIndex :: (Eq a, Num a) => [t] -> a -> [t]
splitIndex [] z = []
splitIndex [x] z = if z == 0 then [x] else []
splitIndex (x:y:[]) z = if z == 0 then [x] else [y]
splitIndex (x:y:xs) z
    | z == 0 =  [x] ++ splitIndex xs z
    | z == 1 = [y] ++ splitIndex xs z
    | otherwise = []

uniqueHouses :: [Char] -> [[Int]]
uniqueHouses text = nub 
    (scanl1 (zipWith (+)) ([0,0]:(map convert text)))

main = do
    contents <- readFile "input.txt"
    print (length (nub (uniqueHouses (splitIndex contents 0) ++ 
        uniqueHouses (splitIndex contents 1))))
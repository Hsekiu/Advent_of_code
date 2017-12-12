import Data.List
import Data.List.Split

area :: [Int] -> Int
area (l:w:h:_) = 2*l*w + 2*w*h + 2*h*l + small [l,w,h]
    where small x =  head (sort (zipWith (*) x ([last x] ++ init x)))

totalArea x = sum (map area 
    ((map.map) (read::String->Int) (map (splitOn "x") x)))

main = do
    contents <- readFile "input.txt"
    print (totalArea (words (contents)))
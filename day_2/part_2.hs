import Data.List
import Data.List.Split

feet :: [Int] -> Int
feet (x:y:z:_) = x*2 + y*2 + x*y*z

totalFeet x = sum (map feet 
	(map sort ((map.map) (read::String->Int) (map (splitOn "x") x))))

main = do
    contents <- readFile "input.txt"
    print (totalFeet (words (contents)))
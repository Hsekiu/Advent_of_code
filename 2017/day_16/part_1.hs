import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe

spin :: Int -> [Char] -> [Char]
spin x y = (reverse (take x (reverse y))) ++ (take ((length y) - x) y)

swap :: Int -> Int -> [Char] -> [Char]
swap x y z = list1 ++ [z !! y] ++ list2 ++ [z !! x] ++ list3
    where list1 = take x z
          list2 = drop (succ x) (take y z)
          list3 = drop (succ y) z

controlIn :: String -> [Char] -> [Char]
controlIn x y | head x == 's' = spin (digitToInt b) y
              | head x == 'x' = swap (digitToInt a) (digitToInt b) y
              | head x == 'p' = swap (findposition a y) (findposition b y) y
              | otherwise = []
    where a = (head (tail x))
          b = (last x)

findposition char = (\(Just i)->i) . findIndex (==char)

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    let instructions = splitOn "," ((input!!0)!!0)
    --print (controlIn "s3" ['a'..'e'])
    print (spin 3 ['a'..'e'])
import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

spin :: Int -> [Char] -> [Char]
spin x y = (reverse (take x (reverse y))) ++ (take ((length y) - x) y)

swap :: Int -> Int -> [Char] -> [Char]
swap x y z = list1 ++ [z !! newy] ++ list2 ++ [z !! newx] ++ list3
    where list1 = take newx z
          list2 = drop (succ newx) (take newy z)
          list3 = drop (succ newy) z
          newx = if x < y then x else y
          newy = if newx /= x then x else y

controlIn :: String -> [Char] -> [Char]
controlIn x y | head x == 's' = spin (read b :: Int) y
              | head x == 'x' = swap (read a :: Int) (read b :: Int) y
              | head x == 'p' = swap (findposition (a!!0) y) (findposition (b!!0) y) y
              | otherwise = []
    where a = head (splitOn "/" (tail x))
          b = last (splitOn "/" (tail x))

instructRun :: [String] -> [Char] -> [Char]
instructRun [x] y = controlIn x y
instructRun (x:xs) y = 
    --trace ("instruction is: " ++ show y ++ " on " ++ show x) $ 
    instructRun xs (controlIn x y)

findposition char = (\(Just i)->i) . findIndex (==char)

countCycle :: [String] -> Int -> Int
countCycle [x] y = 0
countCycle (x:xs) y = if loc == [] then countCycle xs (y + 1) else ((loc!!0) + y + 1)
    where loc = elemIndices x xs

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    let instructions = splitOn "," ((input!!0)!!0)
    let cycle = (countCycle (take 100 (iterate (instructRun instructions) ['a'..'p'])) 0)
    let position =  1000000000 `mod` cycle
    print ((iterate (instructRun instructions) ['a'..'p'])!!position)
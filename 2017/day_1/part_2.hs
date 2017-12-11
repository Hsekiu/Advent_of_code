--Take the two halves of the list and compare and add.
inv :: [Int] -> [Int] -> Int
inv [x] [y] = if (x == y) then (x + y) else 0
inv (x:xs) (y:ys) = if (x == y) then (x + y) + inv xs ys else inv xs ys
 
main = do
    contents <- readFile "input.txt"
    let input = map (read . (:"")) contents :: [Int]
    let half = (length input) `div` 2
    let left = take half input
    let right = drop half input
    let result = inv left right
    print result
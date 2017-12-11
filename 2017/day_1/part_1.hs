inv :: [Int] -> Int
inv (x:y:xs) = if (x == y) then x + inv(y:xs) else inv(y:xs)
inv (x:y) = 0
 
main = do
    contents <- readFile "input.txt"
    let input = map (read . (:"")) contents :: [Int]
    --Add elements that are the same next to each other and edge case of wrap around.
    let result = if head(input) == last(input) then (inv(input)) + head(input) else (inv(input))
    print result
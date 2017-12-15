--Input, position, steps
jump :: [Int] -> Int -> Int -> Int
jump x y z = if (y + pos) < (length x) then jump inc (y + pos ) (z + 1) else z + 1
    where inc = (\y -> (take y x) ++ [off] ++ (drop (y + 1) x)) y
          pos = x!!y --if x!!y < 0 then (((length x) - 1) + x!!y) else x!!y
          off = if x!!y >= 3 then x!!y - 1 else x!!y + 1
          --db = trace ("List is:" ++ show x ++ " position is: " ++ show (pos + y))

main = do
    contents <- readFile "input.txt"
    let input = map read (lines contents) :: [Int]
    print (jump input 0 0)
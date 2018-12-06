--parse input to valid ints
toInt :: String -> Int
toInt (x:xs) = if x == '-' then (-1) * read xs :: Int else read xs :: Int

main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let ans = sum (map toInt input)
    print ans
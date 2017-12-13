spiralToMan :: Int -> Int
spiralToMan x =
    if (last powlist) == x then x
    else last (take (length powlist + 1) [x | x <- (map (\x -> x ^ 2) [1..]), odd x])
    where powlist = takeWhile (<= x) [x | x <- (map (\x -> x ^ 2) [1..]), odd x]

intToMan :: Int -> Int
intToMan x = abs ((cornerdis - 1) - distance)
    where distance = (cornerdis ^ 2) `mod` x
          cornerdis = truncate (sqrt $ fromIntegral (spiralToMan x))

main = do
    print (intToMan 265149)
import System.IO

--Possible recurisve solution
{-
count :: String -> Int
count [] = 0
count (x:xs) = count xs + (if x == '(' then 1 else -1)
-}

main = do
    contents <- readFile "input.txt"
    --print (count contents)

    --replace chars with numbers then sum
    print (sum (map (\x -> if x == '(' then 1 else -1) contents))
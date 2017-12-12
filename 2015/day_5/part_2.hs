import Data.Text hiding (length, map, words, filter)
import Data.List

btwn :: [Char] -> Bool
btwn (x:y:z:[]) = if (x == z) then True else False
btwn (x:y:z:xs) = if (x == z) then True else (btwn ([y] ++ [z] ++ xs))

genComb = [[x] ++ [y] | x <- xs, y <- xs]
    where xs = ['a'..'z'] 

doubles' :: [[Char]] -> [Char] -> Bool
doubles' [x] y = if (length (breakOnAll (pack x) (pack y))) 
    > 1 then True else False
doubles' (x:xs) y = if (length (breakOnAll (pack x) (pack y))) 
    > 1 then True else (doubles' xs y)

doubles x = doubles' genComb x

niceString x = (doubles x) && (btwn x)

main = do
    contents <- readFile "input.txt"
    print (length (filter (==True) (map niceString (words contents))))
import System.IO
import Data.List
import Data.Maybe

main = do
    contents <- readFile "input.txt"

    --create a list of running total sum then find -1
    print (fromJust 
        (elemIndex (-1) 
            (scanl (+) 0 
                (map (\x -> if x == '(' then 1 else -1) contents))))
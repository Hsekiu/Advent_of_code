import Data.List
import Data.List.Split

isValid :: [String] -> Bool
isValid [x] = True
isValid (x:xs) =
    if isDup then False else isValid xs
    where isDup = not (length (nub ((nub (permutations x)) ++ xs)) == length ((nub (permutations x)) ++ xs))

main = do
    contents <- readFile "input.txt"
    let input = map (splitOn " ") (lines contents)
    print (length (filter isValid input))

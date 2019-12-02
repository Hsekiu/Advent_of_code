import Debug.Trace

trace' arg = trace (show arg) arg

calc :: Int -> Int -> Int
calc x add
   | fuel > 0 = calc fuel (add + fuel)
   | otherwise = add
   where fuel = (x `div` 3) - 2

main = do
  contents <- readFile "input.txt"
  let input = map (read::String->Int) (lines contents)
  print $ sum $ map (\x -> calc x 0) input
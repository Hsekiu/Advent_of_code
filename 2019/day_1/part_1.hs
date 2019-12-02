calc :: String -> Int
calc x = (val `div` 3) - 2
  where val = read x :: Int

main = do
  contents <- readFile "input.txt"
  let input = lines contents
  print $ sum $ map calc input
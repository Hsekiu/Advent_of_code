import Data.List

counter :: String -> [Int]
counter hash = filter (>1) $ nub $ map length fun
  where fun = group $ sort hash

main = do
  contents <- readFile "input.txt"
  let input = lines contents
  print $ foldr1 (*) . map length $ group $ sort $ concat $ map counter input
import Data.List.Split
import Data.Sequence

trace' arg = trace (show arg) arg

run :: Seq(Int)-> Int -> Seq(Int)
run tape ind
   | insc == 1 = run (update stor (val1 + val2) tape) (ind+4)
   | insc == 2 = run (update stor (val1 * val2) tape) (ind+4)
   | otherwise = tape
   where insc = index tape ind
         val1 = index tape (index tape (ind+1))
         val2 = index tape (index tape (ind+2))
         stor = index tape (ind+3)

main = do
   contents <- readFile "input.txt"
   --let input = map (read::String->Int) (lines contents)
   let proc = map (read::String->Int) (splitOn "," contents)
   let instructions = fromList proc
   let instructions1 = update 1 12 instructions
   let instructions2 = update 2 2 instructions1
   print $ run instructions2 0
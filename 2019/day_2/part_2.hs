import Data.List.Split
import Data.Sequence
import Debug.Trace

trace' arg = trace (show arg) arg

findval :: Seq(Int) -> Int -> Int -> Int -> Int
findval tape find noun verb
   | val == find = 100 * noun + verb
   | verb == 99 = findval newt find (noun+1) 0
   | noun <= 99 = findval newt find noun (trace' (verb+1))
   | otherwise = 100 * noun + verb
   where val = index (run newt 0) 0
         newt = update 1 noun (update 2 verb tape)

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
   print $ findval instructions 19690720 0 0
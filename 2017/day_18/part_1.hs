{-|
snd X plays a sound with a frequency equal to the value of X.
set X Y sets register X to the value of Y.
add X Y increases register X by the value of Y.
mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)
-}

import Data.List.Split
import Data.List
import Debug.Trace
import Data.Maybe
import Data.Char

findposition char = (\(Just i)->i) . elemIndex char

incRegister :: [(Char, Int)] -> Char -> (Int -> Int) -> [(Char, Int)]
incRegister x y z = 
    if pos < 0 
    then x ++ [(y, z 0)] 
    else list1 ++ [(y, z (snd (ele !! 0)))] ++ list2
        where pos = if ele == [] then -1 else (findposition (ele !! 0) x)
              list1 = fst (splitAt pos x)
              ele = filter (\x->fst x == y) x
              list2 = tail $ snd (splitAt pos x)

runInstruction :: [String] -> Int -> [(Char, Int)] -> Int -> Int
runInstruction x pos regs lastfreq
    | pos >= length x = trace ("Ran out of instructions") $ 0
    | ins == "snd" = trace ("snd") $
        runInstruction x (succ pos) regs (snd ((findreg varx) !! 0))
    | ins == "set" = trace ("set") $
        runInstruction x (succ pos) (incRegister regs (varx !! 0) (\x->x*0 + newvary)) lastfreq
    | ins == "add" = trace ("add") $
        runInstruction x (succ pos) (incRegister regs (varx !! 0) (+newvary)) lastfreq
    | ins == "mul" = trace ("mul") $
        runInstruction x (succ pos) (incRegister regs (varx !! 0) (*newvary)) lastfreq
    | ins == "mod" = trace ("mod") $
        runInstruction x (succ pos) (incRegister regs (varx !! 0) (`mod` newvary)) lastfreq
    | ins == "rcv" = trace ("rcv") $
        if (snd ((findreg varx) !! 0) /= 0)
        then lastfreq
        else runInstruction x (succ pos) regs lastfreq
    | ins == "jgz" = trace ("jgz " ++ show newpos ++ " " ++ show (snd ((findreg varx) !! 0) > 0)) $
        if (snd ((findreg varx) !! 0) > 0)
        then runInstruction x (newpos) regs lastfreq
        else runInstruction x (succ pos) regs lastfreq
    | otherwise = trace ("Instruction not known") $ 0
        where input = splitOn " " (x !! pos) 
              varx = input !! 1
              vary = if (length input /= 3) then "0" else (input !! 2)
              newvary = 
                  if isLetter (vary !! 0)
                  then snd ((findreg vary) !! 0)  
                  else read vary :: Int
              ins = input !! 0
              newpos = if (newvary < 0) then pos + newvary else pos + newvary
              findreg var = filter (\x->fst x == (var !! 0)) regs

main = do
    contents <- readFile "input.txt"
    let input = (lines contents)
    print (runInstruction input 0 [] 0)
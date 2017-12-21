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
    then x ++ [(y, z 1)] 
    else list1 ++ [(y, z (snd (ele !! 0)))] ++ list2
        where pos = if ele == [] then -1 else (findposition (ele !! 0) x)
              list1 = fst (splitAt pos x)
              ele = filter (\x->fst x == y) x
              list2 = tail $ snd (splitAt pos x)

runInstruction :: [String] -> Int -> [(Char, Int)] -> Int
runInstruction x pos regs
    | pos >= length x = trace ("Ran out of instructions") $ 0
    | ins == "snd" = 
        runInstruction x (succ pos) regs --(incRegister regs (snd (findreg!!0)))
    | ins == "set" = 
        runInstruction x (succ pos) (incRegister regs (varx!!0) (\x->x*0 + vary))
    | ins == "add" = 
        runInstruction x (succ pos) (incRegister regs (varx!!0) (+vary))
    | ins == "mul" = 
        runInstruction x (succ pos) (incRegister regs (varx!!0) (*vary))
    | ins == "mod" = 
        runInstruction x (succ pos) (incRegister regs (varx!!0) (`mod` vary)) 
    | ins == "rcv" = 
        if (snd (findreg!!0) /= 0)
        then (snd (findreg!!0))
        else runInstruction x (succ pos) regs
    | ins == "jgz" = 
        if vary > 0
        then runInstruction x (newpos) regs
        else runInstruction x (succ pos) regs
    | otherwise = trace ("Instruction not known") $ 0
        where input = splitOn " " (x !! pos) 
              varx = input !! 1
              vary = if (length input /= 3) then 0 else read (input !! 2) :: Int
              ins = input !! 0
              newpos = if (vary < 0) then pos + vary + 1 else pos + vary - 1
              findreg = filter (\x->fst x == (varx!!0)) regs
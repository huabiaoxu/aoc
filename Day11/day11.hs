import System.Environment

import Data.Char
import Data.List

next lst = 
    let reversed    = reverse lst
        next' []    = []
        next' (x:xs) 
            | x == 25   = 0 : next' xs
            | otherwise = (x + 1) : xs
    in reverse $ next' reversed 

encode c = ((ord c) - (ord 'a')) `mod` 26
decode n = chr (n + (ord 'a'))
        
inc3 [a,b,c] = (b == a+1) && (b == c-1)
lst3 a b c   = [a, b, c]

numDoubles []      = 0
numDoubles (e:[])  = 0
numDoubles (x:y:t)
    | x == y    = 1 + numDoubles t
    | otherwise = numDoubles (y:t)

isValid lst =
    let banned e  = elem e $ map encode "ilo"
        notBanned = and $ map (not . banned) lst 
        threes       = zipWith3 lst3 lst (tail lst) (drop 2 lst)
        hasInc       = or $ map inc3 threes
        doubleDouble = (numDoubles lst) >= 2
    in hasInc &&  notBanned && doubleDouble

main = do
    [input] <- getArgs

    let encoded         = map encode input
        nextLst         = iterate next encoded
        validLst        = filter isValid $ tail nextLst
        decodedLst      = map (map decode) validLst
        [first, second] = take 2 decodedLst

    putStrLn("Next two passwords: " ++ first ++ " and " ++ second)

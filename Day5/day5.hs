import System.Environment
import Data.List.Split
import Data.List

import qualified Data.Map.Strict as Map

isVowel x = elem x "aeiou"

b2i :: Bool -> Int
b2i False = 0
b2i True = 1

threeVowels x = sum(map (b2i . isVowel) x) >= 3

doubled []          = False
doubled (_:[])      = False
doubled lst@(x:y:t)
    | x == y    = True
    | otherwise = doubled(tail lst)

notBanned x = 
    let contains  = (\substr->isInfixOf substr x)
        contained = map contains ["ab", "cd", "pq", "xy"]
    in all (==False) contained

isNice criteria x =
    let satisfies = map (\fn -> fn x) criteria
    in all (==True) satisfies

insertList key value map = case Map.lookup key map of
    Just lst -> Map.insert key (value:lst) map
    Nothing  -> Map.insert key (value:[])  map

hasTwice (x:y:[]) = abs(x - y) > 1
hasTwice lst = length lst > 2

twoTwice x = 
    let twos    = map (\(f, s)-> f:s:[]) (zip x (tail x))
        twosMap = foldl (\map (k, v) -> insertList k v map) Map.empty (zip twos [0..])
    in Map.foldl (||) False (Map.map hasTwice twosMap)

doubleSkip []         = False
doubleSkip (_:[])     = False
doubleSkip (_:_:[])   = False
doubleSkip lst@(x:_:z:t)
    | x == z    = True
    | otherwise = doubleSkip (tail lst)

main = do
    [fname] <- getArgs
    input   <- readFile fname
    let strings = init(splitOn "\n" input)

    print $ length $ filter twoTwice strings
    print $ length $ filter doubleSkip strings
    putStrLn("First criteria: "  ++ (show $ length $ filter (isNice [threeVowels, doubled, notBanned]) strings))
    putStrLn("Second criteria: " ++ (show $ length $ filter (isNice [twoTwice, doubleSkip])            strings))
    

import System.Environment
import Data.List
import Data.List.Split
import Data.Char
import Data.Array

import qualified Data.Map.Strict as Map

data Token = TokFiller
           | TokGain
           | TokLose
           | TokName String
           | TokNum Int
    deriving (Show, Eq)

token x
    | elem x ["would", "happiness", "units", "by", "sitting", "next", "to"] = TokFiller
    | x == "gain"   = TokGain
    | x == "lose"   = TokLose
    | otherwise  = case reads x :: [(Int, String)] of
        [(n, "")] -> TokNum n
        _         -> TokName x
        
isValidTok TokFiller = False
isValidTok x = True

parseNum TokGain (TokNum x) = x
parseNum TokLose (TokNum x) = -x

parseLine [TokName n1, x, y, TokName n2] = 
    let num    = parseNum x y
        sorted = sort [n1, n2]
    in (sorted, num)

initSquare (lb, ub) val = array ((lb,lb),(ub,ub)) [((i,j),val) | i <- [lb..ub], j <- [lb..ub]]

updateTable nameMap t upd = 
    let lookup' k = case Map.lookup k nameMap of
            Just v  -> v
            Nothing -> undefined
        [i, j] = map lookup' $ fst upd
        updVal = (t ! (i,j)) + (snd upd)
    in t // [((i, j), updVal), ((j, i), updVal)]

computeHappiness table lst = 
    let pairs  = zip lst ((drop 1 lst) ++ [head lst])
        values = map (table !) pairs
    in sum(values)

computeHappiness2 table lst = 
    let pairs = zip lst (drop 1 lst)
        values = map (table !) pairs
    in sum values

main = do
    [fname] <- getArgs

    input <- readFile fname

    let instructions = lines input
        tokenized    = map (filter isValidTok . map token . splitOn " " . filter (/= '.')) instructions
        parsed       = map parseLine tokenized
        names        = sort $ nub $ concat $ map fst parsed
        nameMap      = Map.fromList $ zip names [0..]
        emptyTable   = initSquare (0, (length names) - 1) 0
        table        = foldl (updateTable nameMap) emptyTable parsed
        possibles    = map (0:) $ permutations [1..length names - 1]
        possibles2   = permutations [0..length names -1]
        happiness    = map (computeHappiness table) possibles
        happiness2   = map (computeHappiness2 table) possibles2

    print $ maximum happiness
    print $ maximum happiness2

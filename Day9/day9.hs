import System.Environment
import Data.List.Split
import Data.List
import Data.Array
import Data.Bits
import Data.Char

import qualified Data.Map.Strict as Map

data Token = TokIdent String
           | TokNum Int
           | TokTo
           | TokEq
    deriving (Show, Eq)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | c == '='  = TokEq : tokenize cs
    | isSpace c = tokenize cs


alphas :: String -> (String, String)
alphas str = als "" str
    where
        als acc [] = (acc, [])
        als acc (c:cs)
            | isAlpha c = let (acc', cs') = als acc cs
                          in (c:acc', cs')
            | otherwise = (acc, c:cs)

digits :: String -> (String, String)
digits str = digs "" str
    where
        digs :: String -> String -> (String, String)
        digs acc [] = (acc, [])
        digs acc (c:cs) 
            | isDigit c = let (acc', cs') = digs acc cs
                          in (c:acc', cs')
            | otherwise = (acc, c:cs)

identifier c cs =
    let (str, cs') = alphas cs in
    TokIdent (c:str) : tokenize cs'

number c cs = 
    let (digs, cs') = digits cs in
    TokNum (read (c:digs)) : tokenize cs'

transformOp (TokIdent op)
    | op == "to"    = TokTo
transformOp x = x

isIdent (TokIdent _) = True
isIdent _ = False

makeSquare lb ub = array ((lb,lb), (ub,ub)) [((i,j), 0) | i<-[lb..ub], j<-[lb..ub]]

findInMap map k = case Map.lookup k map of
    Just v  -> v
    Nothing -> undefined

processDistance :: Map.Map String Int -> Array (Int,Int) Int -> [Token] -> Array (Int,Int) Int
processDistance idMap arr [TokIdent n1, TokTo, TokIdent n2, TokEq, TokNum dist] =  arr // update 
    where id1 = findInMap idMap n1
          id2 = findInMap idMap n2
          update = [((id1,id2),dist), ((id2,id1),dist)]

computeDistance distanceMat ids =
    let steps = zip ids $ tail ids
        dists = map (distanceMat !) steps
    in sum(dists)

main = do
    [fname] <- getArgs
    input   <- readFile fname

    let instructions           = init(splitOn "\n" input)
        tokenized              = map (map transformOp . tokenize) instructions
        toName (TokIdent name) = name
        uniques                = nub $ map toName $ filter isIdent $ concat tokenized
        idMap                  = Map.fromList (zip uniques [1..])
        distanceMat            = foldl (processDistance idMap) (makeSquare 1 (length uniques)) tokenized
        possibles              = permutations [1..length uniques]
        distances              = map (computeDistance distanceMat) possibles

    putStrLn("min: " ++ show(minimum distances))
    putStrLn("max: " ++ show(maximum distances))

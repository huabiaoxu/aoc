import System.Environment
import Data.List.Split
import Data.List
import Data.Array
import Data.Bits
import Data.Char

import qualified Data.Map.Strict as Map

data Operator = And | Or | LShift | RShift | Not
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokAssign 
    deriving (Show, Eq)

data Graph a  = Graph { vertices :: [a], edges :: [(a, a)] } deriving Show

-- TOKENIZE ---------------------------------------------------------
tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs)
    | isDigit c                  = number c cs
    | isAlpha c                  = identifier c cs
    | c == '-' && head cs == '>' = TokAssign : tokenize (tail cs)
    | isSpace c                  = tokenize cs
    | otherwise                  = error $ "cannot tokenize " ++ [c]

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
    | op == "AND"    = TokOp And
    | op == "OR"     = TokOp Or
    | op == "LSHIFT" = TokOp LShift
    | op == "RSHIFT" = TokOp RShift
    | op == "NOT"    = TokOp Not
transformOp x = x


-- GRAPH --------------------------------------------------------
removeEdge :: (Eq a) => (a, a) -> Graph a -> Graph a
removeEdge x (Graph v e) = Graph v (filter (/=x) e)

connections :: (Eq a) => ((a, a) -> a) -> a -> Graph a -> [(a, a)]
connections f x (Graph _ e) = filter ((==x) . f) e

outbound a = connections fst a
inbound  a = connections snd a

tsort :: (Eq a) => Graph a -> [a]
tsort graph = tsort' [] (noInbound graph) graph where
    noInbound (Graph v e) = filter (flip notElem $ map snd e) v
    tsort' lst []    (Graph _ []) = reverse lst
    tsort' lst []    _            = error "graph has cycle"
    tsort' lst (n:s) g            = tsort' (n:lst) s' g' where
        outEdges = outbound n g
        outNodes = map snd outEdges
        g'       = foldr removeEdge g outEdges
        s'       = s ++ filter (null . flip inbound g') outNodes

-- EVALUATION --------------------------------------------------
eval :: (Map.Map String Int) -> [Token] -> Int
eval _      [TokNum num]         = num
eval values [TokIdent var]       = case Map.lookup var values of
    Just value -> value
    Nothing   -> error ("variable " ++ var ++ " not processed")
eval values [TokOp Not, operand] = complement (eval values [operand])
eval values [lhs, TokOp op, rhs]
    | op == And    = (eval values [lhs]) .&. (eval values [rhs])
    | op == Or     = (eval values [lhs]) .|. (eval values [rhs])
    | op == LShift = shift (eval values [lhs]) (eval values [rhs])
    | op == RShift = shift (eval values [lhs]) (-(eval values [rhs]))

processNode map values v = case Map.lookup v map of
    Just node -> let value = eval values node
                 in Map.insert v value values
    Nothing   -> error ("node " ++ v ++ " not found")

-- MISC
mapItem [expr, [TokIdent name]] = (name, expr)

main = do
    [fname] <- getArgs
    input   <- readFile fname

    let instructions = init(splitOn "\n" input)
        tokenized    = map (map transformOp . tokenize) instructions
        parsed       = map mapItem $ map (splitOn [TokAssign]) tokenized
        parsedMap    = Map.fromList parsed
        graph        = Graph v e where 
            v = nub $ map fst parsed
            e = concat $ map toEdges parsed where
                toEdges (vo, node) = case node of
                    [TokIdent vi]        -> [(vo, vi)]
                    [vi1, TokOp op, vi2] -> concat $ map toEdges [(vo, [vi1]), (vo, [vi2])]
                    [TokOp _, vi]        -> toEdges (vo, [vi])
                    [TokNum _]           -> []
        procOrder    = reverse $ tsort graph
        values       = let processNode' = processNode parsedMap
                       in  foldl processNode' Map.empty procOrder
        aVal         = case Map.lookup "a" values of
            Just value -> value
            Nothing    -> undefined
        overrideMap  = Map.insert "b" [TokNum aVal] parsedMap
        overrideVals = let processNode' = processNode overrideMap
                       in  foldl processNode' Map.empty procOrder
        aValOverride = case Map.lookup "a" overrideVals of
            Just value -> value
            Nothing    -> undefined


    putStrLn("a = "  ++ show(aVal))
    putStrLn("a' = " ++ show(aValOverride))

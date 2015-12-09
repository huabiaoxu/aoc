import System.Environment
import Data.List.Split
import Data.List
import Data.Array
import Data.Bits
import Data.Char

import qualified Data.Map.Strict as Map

data Escapable = EscQuote  | EscHex String | EscSlash
    deriving (Show, Eq)

data Token = TokChar Char
           | TokEsc Escapable
           | TokQuote
    deriving (Show, Eq)

tokenize :: [Char] -> [Token]
tokenize [] = []
tokenize (c:cs)
    | c == '\\' = matchEscape cs
    | c == '"'  = TokQuote  : tokenize cs
    | otherwise = TokChar c : tokenize cs

matchHex x = 1

matchEscape :: [Char] -> [Token]
matchEscape []     = undefined
matchEscape (c:cs)
    | c == '\\' = TokEsc EscSlash     : tokenize cs
    | c == '"'  = TokEsc EscQuote     : tokenize cs
    | c == 'x'  = TokEsc (EscHex hex) : tokenize (drop 2 cs)
        where hex = take 2 cs

litVal :: Token -> Int
litVal (TokChar _)  = 1
litVal (TokEsc esc) = case esc of
    EscQuote -> 2
    EscSlash -> 2
    EscHex _ -> 4
litVal TokQuote   = 1

memVal :: Token -> Int
memVal TokQuote = 0
memVal _ = 1

encVal :: Token -> Int
encVal (TokChar _)  = 1
encVal (TokEsc esc) = case esc of
    EscQuote -> 4
    EscSlash -> 4
    EscHex _ -> 5
encVal TokQuote   = 3

main = do
    [fname] <- getArgs
    input   <- readFile fname

    let strings      = init(splitOn "\n" input)
        tokenized    = map tokenize strings
        litNumChar   = sum $ map litVal $ concat tokenized
        memNumChar   = sum $ map memVal $ concat tokenized
        encNumChar   = sum $ map encVal $ concat tokenized


    putStrLn("num literal - num memory  = " ++ show(litNumChar - memNumChar))
    putStrLn("num encoded - num literal = " ++ show(encNumChar - litNumChar))

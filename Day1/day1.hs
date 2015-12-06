import System.Environment
import Data.List
-- Turns parenthesis to integer values per instructions:
p2i :: Char -> Int
p2i '(' = 1
p2i ')' = -1

findposition number = (\(Just i)->i) . findIndex (==number)

main = do
    [fname]  <- getArgs
    input    <- readFile fname

    let vals   = map p2i input
    let cumsum = scanl1 (+) vals
    let negidx = findposition (-1) cumsum

    putStrLn $ "Final Floor: "    ++ show(last cumsum)
    putStrLn $ "First Basement: " ++ show(negidx + 1)


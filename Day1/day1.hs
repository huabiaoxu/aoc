import System.Environment

-- Turns parenthesis to integer values per instructions:
p2i :: Char -> Int
p2i '(' = 1
p2i ')' = -1

-- Find element, assuming it will be found
findposition number = (\(Just i)->i) . findIndex (==number)

main = do
    [fname]  <- getArgs
    input    <- readFile fname

    let vals   = map p2i input
    let cumsum = scanl1 (+) vals
    let negidx = findposition (-1) cumsum

    putStrLn $ "Final Floor: "    ++ show(last cumsum)
    putStrLn $ "First Basement: " ++ show(negidx + 1)


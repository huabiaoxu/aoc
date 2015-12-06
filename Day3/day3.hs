import System.Environment
import Data.List

-- Turns carets into int^2 values
c2ii :: Char -> (Int, Int)
c2ii 'v' = (0, -1)
c2ii '^' = (0,  1)
c2ii '>' = (1,  0)
c2ii '<' = (-1, 0)

-- Add grid values
tadd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- List index by List
getIndices xs is = map (xs!!) is

main = do
    [fname] <- getArgs
    input   <- readFile fname

    let directions = map c2ii input
        locations = scanl tadd (0,0) directions

    let topIdx = (length directions) - 1
        santaDirections = getIndices directions [0,2..topIdx]
        robotDirections = getIndices directions [1,3..topIdx]
        santaLocations  = scanl tadd (0,0) santaDirections
        robotLocations  = scanl tadd (0,0) robotDirections

    putStrLn("Total unique houses 1st year: " ++  (show $ length $ nub locations))
    putStrLn("Total unique houses 2nd year: " ++  (show $ length $ nub (santaLocations ++ robotLocations)))


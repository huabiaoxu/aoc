import System.Environment
import Data.List.Split
import Data.List
import Data.Array

import qualified Data.Map.Strict as Map

-- String to Int conversion
s2i :: String -> Int
s2i = read

-- 2-elem list -> Tuple
tupify2 :: [a] -> (a,a)
tupify2 [x,y] = (x,y)

parseTail x = 
    let parts  = splitOn " " x
        beg = tupify2 $ map s2i (splitOn "," (head parts))
        end = tupify2 $ map s2i (splitOn "," (last parts))
    in (beg, end)

parseLnBin (stripPrefix "turn on "  -> Just t) = ((\x->1  ), parseTail t)
parseLnBin (stripPrefix "turn off " -> Just t) = ((\x->0  ), parseTail t)
parseLnBin (stripPrefix "toggle "   -> Just t) = ((\x->1-x), parseTail t)

parseLnBright (stripPrefix "turn on "  -> Just t) = ((1+),                     parseTail t)
parseLnBright (stripPrefix "turn off " -> Just t) = (((max 0) . (flip (-) 1)), parseTail t)
parseLnBright (stripPrefix "toggle "   -> Just t) = ((2+),                     parseTail t)

rect2D bounds@((lx,ly), (ux, uy)) v = array bounds [((x,y), v) | x <- [lx..ux], y <- [ly..uy]]

sum2D x = sum [x!(i,j) | i <- range(lx,ux), j <- range(ly,uy)]
    where ((lx,ly), (ux,uy)) = bounds x

processUpdate ds upd = ds // [((i,j), upd_f (ds!(i,j))) | i <- [li..ui], j <- [lj..uj]]
    where (upd_f, ((li,lj), (ui,uj))) = upd

main = do
    [fname] <- getArgs
    input   <- readFile fname

    let instructions = init(splitOn "\n" input)
        parsedBin    = map parseLnBin instructions
        parsedBright = map parseLnBright instructions
        initial      = rect2D ((0,0),(999,999)) 0
        finalBin     = foldl processUpdate initial parsedBin
        finalBright  = foldl processUpdate initial parsedBright

    putStrLn("First instructions: "  ++ (show  $ sum2D finalBin))
    putStrLn("Second instructions: " ++ (show  $ sum2D finalBright))

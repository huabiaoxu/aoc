import System.Environment
import Data.List

data Deer = Deer { speed :: Int, duration :: Int, rest :: Int } deriving (Show)

readInt str = case reads str :: [(Int, String)] of
    [(int, "")] -> int
    _           -> -1

parseDeer line =
    let tokens                  = words line
        [speed, duration, rest] = filter (>=0) $ map readInt tokens
    in Deer speed duration rest

computeDistance time deer = 
    let (quotient, remainder) = divMod time $ (duration deer) + (rest deer)
        marchTime  = (duration deer) * quotient
        sprintTime = min (duration deer) remainder
    in speed deer * (marchTime + sprintTime)

iterateDistance time deer =
    let Deer speed duration rest = deer
        sprintSteps  = replicate duration speed
        restSteps    = replicate rest 0
        iterateSpeed = take time $ cycle (sprintSteps ++ restSteps)
    in scanl1 (+) iterateSpeed

winner distances = 
    let maxdist = maximum distances
    in map snd $ filter (\(d,i) -> d==maxdist) $ zip distances [0..]

main = do
    [fname] <- getArgs
    input   <- lines <$> readFile fname

    let deers          = map parseDeer input
        finalDistances = map (computeDistance 2503) deers
        distances      = transpose $ map (iterateDistance 2503) deers
        winners        = map winner distances
        points         = map length $ group $ sort $ concat winners

    print $ maximum finalDistances
    print $ maximum points

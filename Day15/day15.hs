import System.Environment
import Data.List
import Data.List.Split

data Ingredient = Ingredient { capacity :: Int, durability :: Int, flavor :: Int, texture :: Int, calories :: Int } deriving (Show)

readInt str = case reads str :: [(Int, String)] of
    [(int, "")] -> (int, True)
    _           -> (0, False)

parseIngredient line =
    let tokens                  = filter ((>0) . length) $ splitOneOf " ,:" line
        [capacity, durability, flavor, texture, calories] =  map fst $ filter (snd) $ map readInt tokens
    in Ingredient capacity durability flavor texture calories

genParts :: Int -> Int -> [[Int]]
genParts _ 0 = [[]]
genParts n 1 = [[n]]
genParts n p = [x:xs | x <- [0..n], xs <- genParts (n-x) (p-1)]

computeScore ingredients combination = 
    let zipped = zip ingredients combination
        scoreAttribute ingredient amount func = (func ingredient) * amount
        scoreIngredient (ingredient, amount)  = map (scoreAttribute ingredient amount) [capacity, durability, flavor, texture]
        scores = map scoreIngredient zipped
    in product $ map (max 0 . sum) $ transpose scores

cookieCalories ingredients combination =
    sum $ map product $ transpose [map calories ingredients, combination]

main = do
    [fname] <- getArgs
    input   <- lines <$> readFile fname

    let ingredients  = map parseIngredient input
        possibles    = genParts 100 (length ingredients)
        possibles500 = filter ((==500) . cookieCalories ingredients) possibles
        maxpossible  = maximum $ map (computeScore ingredients) possibles
        max500       = maximum $ map (computeScore ingredients) possibles500

    print maxpossible
    print max500

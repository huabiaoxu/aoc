import System.Environment
import Data.List.Split
import qualified Data.Map.Strict as Map

data Attr = Attr String Int deriving (Show,Eq)
data Aunt = Aunt [Attr]     deriving (Show,Eq)

parseAttr :: [String] -> Attr
parseAttr [name, valStr] =
    let val = read valStr :: Int
    in Attr name val
parseAttr _ = undefined

parseAunt :: [Char] -> (Int, Aunt)
parseAunt line =
    let (_:sue:attrStr) = split (dropDelims . condense $ oneOf ":, ") line
        sueNum          = read sue :: Int
        attrs = map parseAttr $ chunksOf 2 attrStr
    in (sueNum, Aunt attrs)

matchAunt target (Aunt attrs) = 
    let matchAttr (Attr name val) = case Map.lookup name target of
            Just targetFn -> targetFn val
            Nothing       -> error ("cannot find" ++ name)
    in and $ map matchAttr attrs

main = do
    [fname] <- getArgs
    input   <- lines <$> readFile fname

    let reading = [("children", 3), ("cats", 7), ("samoyeds", 2), ("pomeranians", 3), ("akitas", 0),
                   ("vizslas", 0),  ("goldfish", 5),    ("trees", 3),  ("cars", 2), ("perfumes", 1)]
        update  = [("cats", ((<) 7)), ("trees", ((<) 3)), ("pomeranians", ((>) 3)), ("goldfish", ((>) 5))]
        gifter1 = Map.fromList $ map (\(n, v) -> (n, ((==)v))) reading
        gifter2 = foldl (\acc (k, v) -> Map.insert k v acc) gifter1  update
        aunts   = map parseAunt input
        match1  = filter (matchAunt gifter1 . snd) aunts
        match2  = filter (matchAunt gifter2 . snd) aunts

    print match1
    print match2

import System.Environment
import Data.List.Split

-- String to Int conversion
s2i :: String -> Int
s2i = read

volume [x,y,z] = x * y * z
perims [x,y,z] = [2*(x+y), 2*(x+z), 2*(y+z)]
areas  [x,y,z] = [x*y, x*z, y*z]

paperRequired dims =
    let itemAreas = areas(dims)
    in 2 * sum(itemAreas) + minimum(itemAreas)

ribbonRequired dims = 
    let itemVolume = volume(dims)
        itemPerims = perims(dims)
    in minimum(itemPerims) + itemVolume

main = do
    [fname] <- getArgs
    input   <- readFile fname
    let items = map ((map s2i) . splitOn "x") (filter ((>=5) . length) (splitOn "\n" input))

    putStrLn("Total paper required: "  ++  (show $ sum $ map paperRequired items))
    putStrLn("Total ribbon required: " ++  (show $ sum $ map ribbonRequired items))
    

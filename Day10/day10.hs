import System.Environment

import Data.List

lookAndSay numStr = concat $ map readRun compressed
    where groups = group $ numStr
          compressed = zip (map length groups) (map nub groups)
          readRun (count, digit) = (show count)  ++ digit

main = do
    [input] <- getArgs

    let sequence = iterate lookAndSay input
        ans40 = sequence !! 40
        ans50 = sequence !! 50

    putStrLn("length of 40th result is " ++ show(length ans40))
    putStrLn("length of 50th result is " ++ show(length ans50))

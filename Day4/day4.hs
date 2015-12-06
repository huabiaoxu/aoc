import System.Environment
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString as B
import qualified Data.List as L
import Crypto.Hash
import Data.Char

md5 :: String -> Digest MD5
md5 = hashlazy . LB.pack

d2s :: Digest MD5 -> String
d2s x = map (chr . fromEnum) (B.unpack(digestToHexByteString x))

md5s :: String -> String
md5s = d2s . md5

isValidMD5 pre = ((==) pre) . (take (length pre))
num2MD5 pre num = md5s(pre ++ show(num))

findtrue = (\(Just i)->i) . L.findIndex (==True)

main = do
    [input] <- getArgs
    let toMD5 = num2MD5(input)
    putStrLn("Five zeros: " ++ show(findtrue(map (isValidMD5("00000")  . toMD5) [0..])))
    putStrLn("Six zeros: "  ++ show(findtrue(map (isValidMD5("000000") . toMD5) [0..])))
    

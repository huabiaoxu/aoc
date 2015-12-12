import System.Environment
import Data.Aeson
import Data.Scientific
import qualified Data.HashMap.Strict  as H
import qualified Data.Vector          as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T

account :: Value  -> Int
account (String _)    = 0
account (Number num)  = case toBoundedInteger num of
    Just int -> int
    Nothing  -> undefined
account (Array arr)   = V.foldl (accValue account) 0 arr
account (Object map)  = foldl   (accValue account) 0 $ H.elems map
account _             = error "unexpected type in json"

accountRed :: Value -> Int
accountRed (Array arr) = V.foldl (accValue accountRed) 0 arr
accountRed (Object map) =
    let red   = String $ T.pack "red"
        isRed = elem red $ H.elems map
        value = foldl (accValue accountRed) 0 $ H.elems map
    in if isRed then 0 else value
accountRed val = account val

accValue :: (Value -> Int) -> Int -> Value -> Int
accValue acctFunc acc value = acc + (acctFunc value)


main = do
    [fname] <- getArgs
    
    input <- B.readFile fname

    let jsParsed = decode input :: Maybe Value
        js       = case jsParsed of
            Just val -> val
            Nothing  -> undefined
        acct     = account js
        acctRed  = accountRed js

    putStrLn("accounting: " ++ show(acct) ++ ", with red correction: " ++ show(acctRed))

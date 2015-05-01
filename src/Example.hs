module Main where

import Example.Msg
import NSHaskell
import Bindings.Objc
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Control.Monad
import Control.Monad.IO.Class
import Data.List (sort, intercalate)
import Data.Monoid
import Data.Map

main :: IO ()
main = do
    putStrLn "NSHaskell"

    c <- getClass "NSNumber"

    a <- withSelector "numberWithFloat:" $ \numberWithFloat ->
        sendFloatMsg (castPtr c) numberWithFloat 666.0

    s <- description a
    putStrLn s

    return ()



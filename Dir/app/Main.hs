module Main where

import Dir.Service (startApp)
import System.Environment (getArgs)

main :: IO ()
main = do
    port:_ <- getArgs
    startApp (read port)

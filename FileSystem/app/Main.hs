{-# LANGUAGE OverloadedStrings #-}
module Main where

import FileSystem.Service (startApp)
import System.Environment (getArgs)

main :: IO ()
main = do
    port:_ <- getArgs
    startApp (read port)

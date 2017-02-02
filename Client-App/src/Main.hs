module Main where

import Client.API
import Control.Monad.IO.Class
import Data.List.Utils

getL :: ClientState String
getL = liftIO $ getLine

putL :: String -> ClientState ()
putL = liftIO . putStrLn

main :: IO ()
main = do
    putStrLn "username:"
    uname <- getLine
    putStrLn "password:"
    passwrd <- getLine
    runClient (Auth uname passwrd) ("127.0.0.1", 8080) _main
    return ()

_main :: ClientState ()
_main = do
    putL "command:"
    x <- getL
    loop x
     where loop k = do
               let p = lines k
               case head p of
                 "init"    -> initClient
                 "initNew" -> initNewClient
                 "ls"      -> ls $ last p
--                 "open"    -> open $ last p
                 -- "close"   -> close
                 -- "get"     -> get
                 -- "put"     -> put
               _main

ls :: FilePath -> ClientState ()
ls path= do
    contents <- listDir path
    mapM_ putL contents

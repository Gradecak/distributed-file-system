module Main where

import Client.API
import Control.Monad.IO.Class

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
    runNewClient (Auth uname passwrd) ("127.0.0.1", 8080) _main
    return ()

_main :: ClientState ()
_main = do
    putL "command:"
    x <- getL
    loop x
     where loop k = do
               let p = words k
               case head p of
                 "initNew" -> initNewClient
                 "ls"      -> ls $ last p
                 "open"    -> open $ last p
  --               "get"     -> get $ last p
                 x         -> putL x >> _main
                 -- "close"   -> close
                 -- "get"     -> get
                 -- "put"     -> put
               _main

ls :: FilePath -> ClientState ()
ls path= do
    contents <- listDir path
    mapM_ putL contents

--get ::

open :: FilePath -> ClientState ()
open path = do
    fHandle <- openFile path Write
    case fHandle of
      Nothing -> putL "invalid path :("
      Just x -> putL $ show x

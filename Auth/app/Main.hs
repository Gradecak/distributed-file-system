module Main where

import Auth.Service
import System.Environment (lookupEnv)

main :: IO ()
main = do
    authPort <- lookupEnv "AUTH_SERVICE_PORT"
    case authPort of
      Just p -> runAuthService (read p)
      Nothing -> runAuthService 8080 -- fallback on port 8080

module Main where

import Auth
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 8080 app1

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.Service where

import Auth.API
import Auth
import Token
import Token.Generate
import Servant
import Network.CGI (liftIO)
import Network.Wai.Handler.Warp

serveToken :: Auth -> Handler (Token,String)
serveToken (Auth a b ) = do
    authenticated <- liftIO $ authenticate a b
    if authenticated then do
      t <-liftIO $ genToken "src"
      return (t, "12312")
      else throwError err403 {errBody = "Authentication Failure"}

server :: Server AuthAPI
server = serveToken

aApi :: Proxy AuthAPI
aApi = Proxy

app1 :: Application
app1 = serve aApi server

runAuthService :: IO ()
runAuthService = run 8080 app1

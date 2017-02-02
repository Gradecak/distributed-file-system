{-# LANGUAGE OverloadedStrings #-}

module Utils.Error (err423) where

import Servant (ServantErr(..))

err423 :: ServantErr
err423 =  ServantErr { errHTTPCode = 423
                     , errReasonPhrase = "Resource Locked"
                     , errBody = ""
                     , errHeaders = []
                     }

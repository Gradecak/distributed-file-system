{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Data.File (File(..), FileHandle(..), FileMode(..), FileRequest(..)) where

import qualified Control.Applicative    as App
import qualified Control.Monad          as Mon
import           Data.Aeson
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text              as Txt
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           GHC.Generics
--import qualified System.Directory       as Dir

-- | to get around the issue of ByteStrings containing illegal characters
-- | when encoded as Text we will convert the ByteString to Base64 before encoding
instance FromJSON BS.ByteString where
  parseJSON (String t) = pure $ (either (const "") id . B64.decode . encodeUtf8) t
  parseJSON _ = App.empty

instance ToJSON BS.ByteString where
  toJSON = String . decodeUtf8 . B64.encode

-- | File datatype that will facilitate file transfer and version control
data File = File { name     :: String
                 , version  :: Int
                 , filepath :: String
                 , bytes    :: BS.ByteString
                 } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- | Indicates the location of a file on the remote server
data FileHandle = FileHandle  { path     :: FilePath
                              , serverIp :: (String,Int)
                              } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- | our FileMode ADT will be used to Mimick the Unix file access standard
data FileMode = Read | Write | ReadWrite
    deriving (Show)

-- | We must manually provide a ToJSON/FromJSON instances for ADT's
instance ToJSON FileMode where
    toJSON a  = object ["mode" .= Txt.pack (show a)]

instance FromJSON FileMode where
    parseJSON (Object o) = do
        tag <- o .: "mode"
        case (tag :: Txt.Text) of
            "Read"      -> return Read
            "Write"     -> return Write
            "ReadWrite" -> return ReadWrite
            _           -> Mon.mzero

-- | A 'ticket' used to request access to a file
data FileRequest = Request FilePath FileMode
    deriving (Show, Generic, FromJSON, ToJSON)

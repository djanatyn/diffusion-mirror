{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DiffusionMirror where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as B
import           Data.Map
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           Network.Wreq

data Config = Config
  { token  :: Text
  , apiURL :: Text } deriving (Generic, Show)

data Repo = Repo
  { phid :: Text 
  , id   :: Text } deriving Show

instance FromJSON Config

instance FromJSON Repo where
  parseJSON = withObject "response" $ \o -> do
    phid <- o .: "result" >>= (.: "object") >>= (.: "phid")
    id   <- o .: "result" >>= (.: "object") >>= (.: "id")

    return Repo{..}

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- B.readFile path
    maybe (error "could not decode") return $ decode contents

createRepository :: Text -> Config -> IO Repo
createRepository repo config = do
  response <- asJSON =<< getWith opts url 
  return $ response ^. responseBody where
    opts = defaults & param "transactions[0][type]"  .~ ["vcs"]
                    & param "transactions[0][value]" .~ ["git"]
                    & param "transactions[1][type]"  .~ ["name"]
                    & param "transactions[1][value]" .~ [repo]
                    & param "api.token" .~ [token config]
    url = unpack $ apiURL config

main :: IO ()
main = do
  return ()

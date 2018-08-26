{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DiffusionMirror where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Map
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           Network.Wreq

data Config = Config
  { token  :: Text
  , baseURI :: Text } deriving (Generic, Show)

instance FromJSON Config

data Repo = Repo
  { repoPHID :: Text
  , repoID   :: Int } deriving Show

data URI = URI
  { uriID   :: Int
  , uriPHID :: Text
  , uriRepo :: Text
  , uriURL  :: Text } deriving Show

data RepositoryEdit = RepositoryEdit Repo deriving Show

instance FromJSON RepositoryEdit where
  parseJSON = withObject "response" $ \o -> do
    repo <- o .: "result" >>= (.: "object")

    return (RepositoryEdit repo)

instance FromJSON Repo where
  parseJSON = withObject "response" $ \o -> do
    repoPHID <- o .: "phid"
    repoID   <- o .: "id"

    return Repo{..}

instance FromJSON URI where
    parseJSON = withObject "uri" $ \o -> do
      uriID   <- o .: "id"
      uriPHID <- o .: "phid"
      uriURL  <- o .: "fields" >>= (.: "uri") >>= (.: "effective")
      uriRepo <- o .: "fields" >>= (.: "repositoryPHID")

      return URI{..}

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- B.readFile path
    maybe (error "could not decode") return $ decode contents

createRepository :: Text -> Config -> IO RepositoryEdit
createRepository repo config = do
  response <- asJSON =<< getWith opts url
  return $ response ^. responseBody where
    opts = defaults & param "transactions[0][type]"  .~ ["vcs"]
                    & param "transactions[0][value]" .~ ["git"]
                    & param "transactions[1][type]"  .~ ["name"]
                    & param "transactions[1][value]" .~ [repo]
                    & param "api.token" .~ [token config]
    url = (unpack $ baseURI config) ++ "/api/diffusion.repository.edit"

-- getURIs :: [Repo] -> Config -> IO (Response (Map String Value))
-- getURIs phids config = do
--   response <- asJSON =<< getWith opts url
--   return $ response ^. responseBody where
--     url  = unpack $ apiURL config
--     opts = defaults & param "constraints[phids]" .~ [show $ fmap unpack phids]
--                     & param "attachments[uris]"  .~ ["true"]
--                     & param "api.token"          .~ [token config]

main :: IO ()
main = do
  return ()

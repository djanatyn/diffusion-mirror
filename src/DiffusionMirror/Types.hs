{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DiffusionMirror.Types where

import           Control.Monad
import           Data.Aeson
import           Data.Text
import           GHC.Generics

data MirrorRepo = MirrorRepo
  { mirrorURI  :: Text
  , mirrorName :: Text } deriving (Generic, Show)

instance FromJSON MirrorRepo

data Config = Config
  { token   :: Text
  , mirrors :: [MirrorRepo]
  , baseURI :: Text } deriving (Generic, Show)

instance FromJSON Config

data Repo = Repo
  { repoPHID :: Text
  , repoID   :: Int } deriving Show

instance FromJSON Repo where
  parseJSON = withObject "response" $ \o -> do
    repoPHID <- o .: "phid"
    repoID   <- o .: "id"

    return Repo{..}

data URI = URI
  { uriID   :: Text
  , uriPHID :: Text
  , uriRepo :: Text
  , uriURL  :: Text } deriving Show

instance FromJSON URI where
  parseJSON = withObject "uri" $ \o -> do
    uriID   <- o .: "id"
    uriPHID <- o .: "phid"
    uriURL  <- o .: "fields" >>= (.: "uri") >>= (.: "effective")
    uriRepo <- o .: "fields" >>= (.: "repositoryPHID")

    return URI{..}

data RepositoryEdit = RepositoryEdit
  { repo :: Repo } deriving Show

instance FromJSON RepositoryEdit where
  parseJSON = withObject "response" $ \o -> do
    repo <- o .: "result" >>= (.: "object")

    return RepositoryEdit{..}

data URIEdit = URIEdit
  { uris :: [URI] } deriving Show

instance FromJSON URIEdit where
  parseJSON = withObject "response" $ \o -> do
    results <- o .: "result" >>= (.: "data")
    uris    <- mapM ((.: "attachments") >=> (.: "uris") >=> (.: "uris")) results

    return URIEdit{uris = (join uris)}


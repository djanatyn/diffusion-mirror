{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DiffusionMirror where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           Network.Wreq

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

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- BL.readFile path
    maybe (error "could not decode") return $ decode contents

createRepository :: Config -> Text -> IO RepositoryEdit
createRepository config repo = do
  response <- asJSON =<< getWith opts url
  return $ response ^. responseBody where
    opts = defaults & param "transactions[0][type]"  .~ ["vcs"]
                    & param "transactions[0][value]" .~ ["git"]
                    & param "transactions[1][type]"  .~ ["name"]
                    & param "transactions[1][value]" .~ [repo]
                    & param "api.token" .~ [token config]
    url = (unpack $ baseURI config) ++ "/api/diffusion.repository.edit"

-- for phabricator's API requirements
genParameters :: String -> [B.ByteString]
genParameters p = fmap (\n -> B.pack $ "constraints[" ++ p ++ "][" ++ show n ++  "]") [0..]

getURIs :: Config -> [Repo] -> IO URIEdit
getURIs config repos = do
  response <- asJSON =<< post url args
  return $ response ^. responseBody where
    url         = (unpack $ baseURI config) ++ "/api/diffusion.repository.search"
    constraints = Prelude.zipWith (:=) (genParameters "phids") (fmap repoPHID repos)
    args        =
      [ "api.token" := token config
      , "attachments[uris]" := B.pack "True" ] ++ constraints

makeReadOnly :: Config -> URI -> IO BL.ByteString
makeReadOnly config uri = do
  response <- post url args 
  return $ response ^. responseBody where
    url = (unpack $ baseURI config) ++ "/api/diffusion.uri.edit"
    args =
      [ "api.token" := token config
      , "objectIdentifier" := (uriPHID uri)
      , "transactions[0][type]" := B.pack "io" 
      , "transactions[0][value]" := B.pack "read" ]

createMirror :: Config -> MirrorRepo -> IO ()
createMirror config mirror = do
  pRepo <- repo <$> createRepository config (mirrorName mirror)
  pURIs <- uris <$> getURIs config [pRepo]

  mapM_ (makeReadOnly config) pURIs

main :: IO ()
main = do
  return ()

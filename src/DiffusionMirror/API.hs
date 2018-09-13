{-# LANGUAGE OverloadedStrings #-}

module DiffusionMirror.API where


import DiffusionMirror.Types

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as BL
import           Data.Text
import           Network.Wreq

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


setMirrorURI :: Config -> (MirrorRepo, Repo) -> IO BL.ByteString
setMirrorURI config (mirrorRepo, repo) = do
  response <- post url args
  return $ response ^. responseBody where
    url  = (unpack $ baseURI config) ++ "/api/diffusion.uri.edit"
    args =
      [ "api.token"              := token config
      , "transactions[0][type]"  := B.pack "repository"
      , "transactions[0][value]" := (repoPHID repo)
      , "transactions[1][type]"  := B.pack "uri"
      , "transactions[1][value]" := mirrorURI mirrorRepo
      , "transactions[2][type]"  := B.pack "io"
      , "transactions[2][value]" := B.pack "observe" ]

setURIReadOnly :: Config -> URI -> IO BL.ByteString
setURIReadOnly config uri = do
  response <- post url args
  return $ response ^. responseBody where
    url = (unpack $ baseURI config) ++ "/api/diffusion.uri.edit"
    args =
      [ "api.token" := token config
      , "objectIdentifier" := (uriPHID uri)
      , "transactions[0][type]" := B.pack "io"
      , "transactions[0][value]" := B.pack "read" ]

activateRepository :: Config -> Repo -> IO BL.ByteString
activateRepository config repo = do
  response <- post url args
  return $ response ^. responseBody where
    url = (unpack $ baseURI config) ++ "/api/diffusion.repository.edit"
    args =
      [ "api.token" := token config
      , "objectIdentifier" := (repoPHID repo)
      , "transactions[0][type]" := B.pack "status"
      , "transactions[0][value]" := B.pack "active" ]



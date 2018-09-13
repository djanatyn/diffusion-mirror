module DiffusionMirror where

import           DiffusionMirror.API
import           DiffusionMirror.Config
import           DiffusionMirror.Types

import           Control.Monad

setReposReadOnly :: Config -> [Repo] -> IO ()
setReposReadOnly config repos = do
  repoURIs <- uris <$> getURIs config repos
  mapM_ (setURIReadOnly config) repoURIs

createMirrors :: Config -> IO [Repo]
createMirrors config = do
  pRepos <- mapM (createRepository config . mirrorName) (mirrors config)
  setReposReadOnly config (repo <$> pRepos)

  return $ repo <$> pRepos

mirrorConfig :: Config -> IO [Repo]
mirrorConfig config = do
  newRepos <- createMirrors config
  mapM_ (setMirrorURI config) $ zip (mirrors config) newRepos
  mapM_ (activateRepository config) $ newRepos

  return newRepos

main :: IO ()
main = do
  return ()

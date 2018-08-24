{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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

instance FromJSON Config

apiOpts :: Network.Wreq.Options
apiOpts = defaults

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- B.readFile path
    maybe (error "could not decode") return $ decode contents

createRepository :: Text -> Config -> IO (Response (Map String Value))
createRepository repo config = asJSON =<< getWith opts url where
  opts = apiOpts & param "transactions[0][type]"  .~ ["vcs"]
                 & param "transactions[0][value]" .~ ["git"]
                 & param "transactions[1][type]"  .~ ["name"]
                 & param "transactions[1][value]" .~ [repo]
                 & param "api.token" .~ [token config]
  url = unpack $ apiURL config

main :: IO ()
main = do
  return ()

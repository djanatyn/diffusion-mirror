module DiffusionMirror.Config where

import           DiffusionMirror.Types

import           Data.Aeson
import qualified Data.ByteString.Lazy  as BL
import           Data.Maybe

loadConfig :: FilePath -> IO Config
loadConfig path = do
    contents <- BL.readFile path
    maybe (error "could not decode") return $ decode contents

{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LB8
import qualified Data.Text                     as T
import qualified Text.Read                     as R
import qualified Data.Configurator.Types       as C
import           Data.Aeson


type Handler = Request -> IO Response

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> LB8.pack err)

notFoundResponse :: IO Response
notFoundResponse = pure
  $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: IO Response
hasNoPermissionResponse = notFoundResponse

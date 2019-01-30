{-# LANGUAGE OverloadedStrings #-}
module Handlers.Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Data.Aeson
import           Queries.Author
import           Queries.Tag
import           Queries.User
import           Queries.Category
import           Serializers.User
import           Serializers.Category
import           Serializers.Tag
import           Serializers.Author
import           Models.Category
import           Helpers
type Handler = Request -> IO Response

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> BC.pack err)

notFoundResponse :: IO Response
notFoundResponse =
  pure $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: IO Response
hasNoPermissionResponse = notFoundResponse
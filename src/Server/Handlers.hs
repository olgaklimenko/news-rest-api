{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Data.Aeson

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
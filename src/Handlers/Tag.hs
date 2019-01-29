{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tag where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Data.Aeson
import           Queries.Tag
import           Serializers.Tag
import           Helpers
import Handlers.Handlers

createTagHandler :: Handler
createTagHandler req = do
  body <- requestBody req
  let tagData =
        eitherDecode $ LB.fromStrict body :: Either String CreateTagRequest
  either (pure . reportParseError) addTag tagData
 where
  addTag tagData = do
    tag <- createTag $ requestToTag tagData
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON

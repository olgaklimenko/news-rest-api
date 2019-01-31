{-# LANGUAGE OverloadedStrings #-}

module Handlers.News where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Data.Aeson
import           Server.Database
import           Server.Handlers
import           Server.Helpers
import           Queries.News
import           Models.News
import           Serializers.News

createNewsHandler :: Handler
createNewsHandler req = do
    body <- requestBody req
    let newsData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateNewsRequest
    either (pure . reportParseError) addNews newsData
  where
    addNews newsData = do
        news <- createNews $ requestToNews newsData
        let categoryJSON = encode $ newsToResponse news
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           categoryJSON

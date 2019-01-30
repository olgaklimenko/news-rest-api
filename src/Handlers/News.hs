{-# LANGUAGE OverloadedStrings #-}

module Handlers.News where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Data.Aeson
import           Queries.News
import           Models.News
import           Serializers.News
import           Helpers
import           Handlers.Handlers

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

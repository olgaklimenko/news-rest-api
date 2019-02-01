{-# LANGUAGE OverloadedStrings #-}

module Handlers.News where

import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Database
import           Server.Handlers
import           Server.Helpers
import           Queries.News
import           Models.News
import           Serializers.News

createNewsHandler :: Handler
createNewsHandler = do
    req <- asks hRequest
    conn <- asks hConn
    body <- liftIO $ requestBody req
    let newsData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateNewsRequest
    either (pure . reportParseError) (addNews conn) newsData
  where
    addNews conn newsData = do
        news <- liftIO $ createNews conn $ requestToNews newsData
        let categoryJSON = encode $ newsToResponse news
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           categoryJSON

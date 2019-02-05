{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tag where

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
import           Queries.Tag
import           Serializers.Tag
import           Server.Config
import           Server.Pagination
import           Data.Proxy
import           Models.Tag

createTagHandler :: Handler
createTagHandler = do
  req  <- asks hRequest
  conn <- asks hConn
  body <- liftIO $ requestBody req
  let tagData = eitherDecode $ LB.fromStrict body :: Either String TagRequest
  either (pure . reportParseError) (addTag conn) tagData
 where
  addTag conn tagData = do
    tag <- liftIO $ createTag conn (requestToTag tagData)
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON

updateTagHandler :: Handler
updateTagHandler = do
  req  <- asks hRequest
  conn <- asks hConn
  either invalidIdResponse
         (successResponse req conn)
         (getIdFromUrl (Proxy :: Proxy Tag) (pathInfo req) >>= textToInteger)
 where
  successResponse req conn tagId = do
    body <- liftIO $ requestBody req
    let tagData = eitherDecode $ LB.fromStrict body :: Either String TagRequest
    either (pure . reportParseError) updateTagFields tagData
   where
    updateTagFields tagData = do
      tag <- liftIO $ updateTag conn tagId $ requestToTag tagData
      let tagJSON = encode $ tagToResponse tag
      pure
        $ responseLBS status200 [("Content-Type", "application/json")] tagJSON


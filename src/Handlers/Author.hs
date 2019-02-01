{-# LANGUAGE OverloadedStrings #-}
module Handlers.Author where

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
import           Queries.Author
import           Serializers.Author
import           Control.Monad.Reader
import           Control.Monad.IO.Class

getAuthorsListHandler :: Handler
getAuthorsListHandler = do
    conn            <- asks hConn
    usersAndAuthors <- liftIO $ getAuthorsList conn
    let authors          = authorToResponse <$> usersAndAuthors
        printableAuthors = encode authors
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       printableAuthors


createAuthorHandler :: Handler
createAuthorHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    body <- liftIO $ requestBody req
    let createAuthorData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateAuthorRequest
    liftIO $ either reportParseError (createAuthor conn) createAuthorData
  where
    createAuthor conn authorData = do
        (user, author) <- liftIO
            $ addAuthorToDB conn (requestToAuthor authorData)
        let authorJSON = encode $ authorToResponse (user, author)
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           authorJSON
    reportParseError err = pure $ responseLBS
        status400
        [("Content-Type", "plain/text")]
        ("Parse error: " <> BC.pack err)

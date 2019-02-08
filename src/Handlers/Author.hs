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
import           Server.Config
import           Server.Pagination
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import           Data.Proxy
import           Models.Author                  ( Author )
import           Models.User                    ( User )
import           Server.Helpers                 ( textToInteger )

createAuthorHandler :: Handler
createAuthorHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    body <- liftIO $ requestBody req
    let createAuthorData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateAuthorRequest
    either reportParseError (createAuthor conn) createAuthorData
  where
    createAuthor conn authorData = do
        (user, author) <- liftIO
            $ addAuthorToDB conn (requestToCreateAuthor authorData)
        let authorJSON = encode $ authorToResponse (author, user)

        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           authorJSON
    reportParseError err = pure $ responseLBS
        status400
        [("Content-Type", "plain/text")]
        ("Parse error: " <> BC.pack err)

updateAuthorHandler :: Handler
updateAuthorHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    either
        invalidIdResponse
        (successResponse req conn)
        (   getIdFromUrl (Proxy :: Proxy (Author, User)) (pathInfo req)
        >>= textToInteger
        )
  where
    successResponse req conn authorId = do
        body <- liftIO $ requestBody req
        let
            aData =
                eitherDecode $ LB.fromStrict body :: Either
                        String
                        UpdateAuthorRequest
        either (pure . reportParseError) (updateAuthorFields conn) aData
      where
        updateAuthorFields conn aData = do
            result <-
                liftIO $ updateAuthor conn authorId $ requestToUpdateAuthor
                    aData
            
            let authorJSON = encode $ authorToResponse result
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               authorJSON

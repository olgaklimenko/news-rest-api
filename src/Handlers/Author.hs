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
import           Models.Author
import           Serializers.Author
import           Server.Config
import           Server.Pagination

getAuthorsListHandler :: C.Config -> Handler
getAuthorsListHandler conf req = do
    maxLimit <- Limit <$> getConf conf "pagination.max_limit"
    let pagination = getLimitOffset maxLimit req
    conn            <- connectDB conf
    usersAndAuthors <- select conn pagination
    let authors          = authorToResponse <$> usersAndAuthors
        printableAuthors = encode authors
    putStrLn "Students page accessed"
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       printableAuthors

createAuthorHandler :: C.Config -> Handler
createAuthorHandler conf req = do
    body <- requestBody req
    let createAuthorData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateAuthorRequest
    either reportParseError createAuthor createAuthorData
  where
    createAuthor authorData = do
        (user, author) <- addAuthorToDB conf (requestToAuthor authorData)
        let authorJSON = encode $ authorToResponse (author, user)
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           authorJSON
    reportParseError err = pure $ responseLBS
        status400
        [("Content-Type", "plain/text")]
        ("Parse error: " <> BC.pack err)

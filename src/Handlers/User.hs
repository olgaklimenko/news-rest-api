{-# LANGUAGE OverloadedStrings #-}


module Handlers.User where

import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Data.Aeson
import           Server.Database
import           Server.Helpers
import           Server.Handlers
import           Queries.User
import           Serializers.User
import           Server.Pagination
import           Server.Config
import           Data.Proxy
import           Models.User

createUserHandler :: Handler
createUserHandler = do
    req  <- asks hRequest
    conn <- asks hConn
    body <- liftIO $ requestBody req
    let createUserData =
            eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
    liftIO $ either (pure . reportParseError) (createUser conn) createUserData
  where
    createUser conn userData = do
        user <- addUserToDB conn (requestToUser userData)
        let userJSON = encode $ userToResponse user
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           userJSON

updateUserHandler :: Handler
updateUserHandler = do
    req  <- asks hRequest
    conn <- asks hConn
    either
        invalidIdResponse
        (successResponse req conn)
        (getIdFromUrl (Proxy :: Proxy User) (pathInfo req) >>= textToInteger)
  where
    successResponse req conn uId = do
        body <- liftIO $ requestBody req
        let
            uData =
                eitherDecode $ LB.fromStrict body :: Either
                        String
                        UpdateUserRequest
        either (pure . reportParseError) goUpdateUser uData
      where
        goUpdateUser uData = do
            user <- liftIO $ updateUser conn uId $ requestToUpdateUser uData
            let userJSON = encode $ userToResponse user
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               userJSON

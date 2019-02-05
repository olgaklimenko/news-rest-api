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

getUserIdFromUrl :: [T.Text] -> Either String T.Text
getUserIdFromUrl ["api", "users", userId] = Right userId
getUserIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateUserHandler :: Handler
updateUserHandler = do
    req  <- asks hRequest
    conn <- asks hConn
    body <- liftIO $ requestBody req
    let updateUserData =
            eitherDecode $ LB.fromStrict body :: Either String UpdateUserRequest
        userId = either error id (getUserIdFromUrl $ pathInfo req)

    liftIO $ either (pure . reportParseError)
                    (goUpdateUser conn userId)
                    updateUserData
  where
    goUpdateUser conn uid userData = do
        let partial = requestToUpdateUser userData
        user <- liftIO $ updateUser conn uid partial
        let userJSON = encode $ userToResponse user
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           userJSON

{-# LANGUAGE OverloadedStrings #-}
module Handlers.User where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Data.Aeson
import           Queries.User
import           Serializers.User
import           Helpers
import           Handlers.Handlers

createUserHandler :: Handler
createUserHandler req = do
  body <- requestBody req
  let createUserData =
        eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
  either (pure . reportParseError) createUser createUserData
 where
  createUser userData = do
    user <- addUserToDB $ requestToUser userData
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON

getUserIdFromUrl :: [T.Text] -> Either String T.Text
getUserIdFromUrl ["api", "users", userId] = Right userId
getUserIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateUserHandler :: Handler
updateUserHandler req = do
    body <- requestBody req
    let updateUserData = eitherDecode $ LB.fromStrict body :: Either String UpdateUserRequest
        userId = either error id (getUserIdFromUrl $ pathInfo req)

    either (pure . reportParseError) (goUpdateUser userId) updateUserData
    where
    goUpdateUser :: T.Text -> UpdateUserRequest -> IO Response
    goUpdateUser uid userData = do
        let partial = requestToUpdateUser userData
        user <- updateUser uid partial
        let userJSON = encode $ userToResponse user
        pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON
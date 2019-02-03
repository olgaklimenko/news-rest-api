{-# LANGUAGE OverloadedStrings #-}
module Handlers.User where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Database
import           Server.Helpers
import           Server.Handlers
import           Queries.User
import           Serializers.User
import           Server.Pagination
import           Server.Config


createUserHandler :: C.Config -> Handler
createUserHandler conf req = do
    body <- requestBody req
    let createUserData =
            eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
    either (pure . reportParseError) createUser createUserData
  where
    createUser userData = do
        user <- addUserToDB conf (requestToUser userData)
        let userJSON = encode $ userToResponse user
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           userJSON

getUserIdFromUrl :: [T.Text] -> Either String T.Text
getUserIdFromUrl ["api", "users", userId] = Right userId
getUserIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

updateUserHandler :: C.Config -> Handler
updateUserHandler conf req = do
    body <- requestBody req
    let updateUserData =
            eitherDecode $ LB.fromStrict body :: Either String UpdateUserRequest
        userId = either error id (getUserIdFromUrl $ pathInfo req)

    either (pure . reportParseError) (goUpdateUser userId) updateUserData
  where
    goUpdateUser :: T.Text -> UpdateUserRequest -> IO Response
    goUpdateUser uid userData = do
        let partial = requestToUpdateUser userData
        user <- updateUser conf uid partial
        let userJSON = encode $ userToResponse user
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           userJSON

deleteUserHandler :: C.Config -> Handler
deleteUserHandler conf req = either
    invalidIdResponse
    successResponse
    (getUserIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse uId = do
        deleted <- deleteUser conf uId
        case deleted of
            0 -> notFoundResponse
            _ -> pure $ responseLBS status204
                                    [("Content-Type", "application/json")]
                                    ""
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

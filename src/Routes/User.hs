{-# LANGUAGE OverloadedStrings #-}

module Routes.User where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Server.Routes
import           Server.Handlers
import           Handlers.User
import           Serializers.User
import           Data.Proxy
import           Models.User

userRoutes :: [(Route, Handler)]
userRoutes =
    [ (createUserRoute  , createUserHandler)
    , (updateUserRoute  , updateUserHandler)
    , (getUsersListRoute, list userToResponse)
    , (deleteUserRoute  , remove (Proxy :: Proxy User))
    ]

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

updateUserRoute :: Route
updateUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

getUsersListRoute :: Route
getUsersListRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"

deleteUserRoute :: Route
deleteUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute
        "DELETE"

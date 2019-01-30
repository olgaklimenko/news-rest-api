{-# LANGUAGE OverloadedStrings #-}

module Routes.User where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Routes.Routes
import           Handlers.User
import           Handlers.Handlers

userRoutes :: [(Route, Handler)]
userRoutes =
    [
        (createUserRoute, createUserHandler)
        , (updateUserRoute, updateUserHandler)
        , (getUsersListRoute, getUsersListHandler)
    ]

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

updateUserRoute :: Route
updateUserRoute =
    PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

getUsersListRoute :: Route
getUsersListRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "GET"
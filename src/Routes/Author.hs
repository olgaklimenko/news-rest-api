{-# LANGUAGE OverloadedStrings #-}

module Routes.Author where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Server.Routes
import           Server.Handlers
import           Handlers.Author
import           Handlers.User
import           Serializers.Author
import           Data.Proxy
import           Models.Author
import           Models.User

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

updateAuthorRoute :: Route
updateAuthorRoute =
    PathRoute "api" $ PathRoute "authors" $ DynamicRoute "pk" $ MethodRoute
        "PATCH"

deleteAuthorRoute :: Route
deleteAuthorRoute =
    PathRoute "api" $ PathRoute "authors" $ DynamicRoute "pk" $ MethodRoute
        "DELETE"

authorRoutes :: [(Route, Handler)]
authorRoutes =
    [ (createAuthorRoute  , createAuthorHandler)
    , (getAuthorsListRoute, list authorToResponse)
    , (updateAuthorRoute  , updateAuthorHandler)
    , (deleteAuthorRoute  , remove (Proxy :: Proxy (Author, User)))
    ]

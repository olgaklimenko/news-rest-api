{-# LANGUAGE OverloadedStrings #-}

module Routes.Tag where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Server.Routes
import           Server.Handlers
import           Handlers.Tag
import           Serializers.Tag
import           Data.Proxy
import           Models.Tag

tagRoutes :: [(Route, Handler)]
tagRoutes =
    [ (createTagRoute  , createTagHandler)
    , (updateTagRoute  , updateTagHandler)
    , (getTagsListRoute, list tagToResponse)
    , (deleteTagRoute  , remove (Proxy :: Proxy Tag))
    ]

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "POST"

updateTagRoute :: Route
updateTagRoute =
    PathRoute "api" $ PathRoute "tags" $ DynamicRoute "pk" $ MethodRoute "PATCH"

getTagsListRoute :: Route
getTagsListRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "GET"

deleteTagRoute :: Route
deleteTagRoute =
    PathRoute "api" $ PathRoute "tags" $ DynamicRoute "pk" $ MethodRoute
        "DELETE"

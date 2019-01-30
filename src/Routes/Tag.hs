{-# LANGUAGE OverloadedStrings #-}

module Routes.Tag where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Routes.Routes
import           Handlers.Tag
import           Handlers.Handlers

tagRoutes :: [(Route, Handler)]
tagRoutes = [
    (createTagRoute, createTagHandler)
    , (updateTagRoute, updateTagHandler)
    , (getTagsListRoute, getTagsListHandler)
    , (deleteTagRoute, deleteTagHandler)
    ]

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "POST"

updateTagRoute :: Route
updateTagRoute = PathRoute "api" $ PathRoute "tags" $  DynamicRoute "pk" $ MethodRoute "PATCH"

getTagsListRoute :: Route
getTagsListRoute =
    PathRoute "api" $ PathRoute "tags" $ MethodRoute "GET"

deleteTagRoute :: Route
deleteTagRoute =
    PathRoute "api" $ PathRoute "tags" $ DynamicRoute "pk" $ MethodRoute "DELETE"
{-# LANGUAGE OverloadedStrings #-}

module Routes.Tag where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Server.Routes
import           Server.Handlers
import           Handlers.Tag

tagRoutes :: C.Config -> [(Route, Handler)]
tagRoutes conf =
    [ (createTagRoute  , createTagHandler conf)
    , (updateTagRoute  , updateTagHandler conf)
    , (getTagsListRoute, getTagsListHandler conf)
    , (deleteTagRoute  , deleteTagHandler conf)
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

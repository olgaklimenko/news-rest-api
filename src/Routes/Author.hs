{-# LANGUAGE OverloadedStrings #-}

module Routes.Author where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Server.Routes
import           Server.Handlers
import           Handlers.Author


createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

authorRoutes :: C.Config -> [(Route, Handler)]
authorRoutes conf =
    [ (createAuthorRoute  , createAuthorHandler conf)
    , (getAuthorsListRoute, getAuthorsListHandler conf)
    ]

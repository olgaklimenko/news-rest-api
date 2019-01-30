{-# LANGUAGE OverloadedStrings #-}

module Routes.Author where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Routes.Routes
import           Handlers.Author
import           Handlers.Handlers


createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

authorRoutes :: [(Route, Handler)]
authorRoutes =
    [ (createAuthorRoute  , createAuthorHandler)
    , (getAuthorsListRoute, getAuthorsListHandler)
    ]
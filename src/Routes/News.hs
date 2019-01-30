{-# LANGUAGE OverloadedStrings #-}

module Routes.News where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Routes.Routes
import           Handlers.News
import           Handlers.Handlers

newsRoutes :: [(Route, Handler)]
newsRoutes = [(createNewsRoute, createNewsHandler)]

createNewsRoute :: Route
createNewsRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "POST"


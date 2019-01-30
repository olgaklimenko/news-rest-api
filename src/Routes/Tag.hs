{-# LANGUAGE OverloadedStrings #-}

module Routes.Tag where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Routes.Routes
import           Handlers.Tag
import           Handlers.Handlers

tagRoutes :: [(Route, Handler)]
tagRoutes = [(createTagRoute, createTagHandler)]

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "POST"

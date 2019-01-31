{-# LANGUAGE OverloadedStrings #-}

module Routes.News where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Server.Routes
import           Server.Handlers
import           Handlers.News

newsRoutes :: C.Config -> [(Route, Handler)]
newsRoutes conf = [(createNewsRoute, createNewsHandler conf)]

createNewsRoute :: Route
createNewsRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "POST"

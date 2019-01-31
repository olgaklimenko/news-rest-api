{-# LANGUAGE OverloadedStrings #-}

module Routes.RoutingTable where
import           Server.Routes
import           Server.Handlers
import Routes.Author
import Routes.Category
import Routes.User
import Routes.News
import Routes.Tag

import           Network.Wai
import           Network.HTTP.Types

routingTable :: [(Route, Handler)]
routingTable = authorRoutes ++ categoryRoutes ++ userRoutes ++ newsRoutes ++ tagRoutes ++
    [
  ( MethodRoute "GET"
    , const $ pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]


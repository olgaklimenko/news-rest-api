{-# LANGUAGE OverloadedStrings #-}

module Routes.RoutingTable where
import qualified Data.Configurator.Types       as C
import           Server.Routes
import           Server.Handlers
import           Routes.Author
import           Routes.Category
import           Routes.User
import           Routes.News
import           Routes.Tag

import           Network.Wai
import           Network.HTTP.Types

routingTable :: C.Config -> [(Route, Handler)]
routingTable conf =
  authorRoutes conf
    ++ categoryRoutes conf
    ++ userRoutes conf
    ++ newsRoutes conf
    ++ tagRoutes conf
    ++ [ ( MethodRoute "GET"
         , const $ pure $ responseLBS status200
                                      [("Content-Type", "text/html")]
                                      "Ok"
         )
       ]

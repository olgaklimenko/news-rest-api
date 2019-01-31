{-# LANGUAGE OverloadedStrings #-}

module Server.Router where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Network.Wai
import           Network.HTTP.Types
import           Server.Handlers
import           Server.Helpers
import           Server.Middlewares
import           Server.Routes
import           Handlers.User
import           Handlers.Author
import           Handlers.Tag
import           Handlers.Category
import           Handlers.News
import           Queries.Tag

isCorrectRoute :: Route -> [T.Text] -> BS.ByteString -> Bool
isCorrectRoute (MethodRoute x) [] method | x == method = True
                                         | otherwise   = False
isCorrectRoute (MethodRoute x) xs method = False
isCorrectRoute route           [] _      = False
isCorrectRoute (PathRoute s route) (x : xs) method
  | x == s    = isCorrectRoute route xs method
  | otherwise = False
isCorrectRoute (DynamicRoute s route) (x : xs) method =
  isCorrectRoute route xs method


route :: [(Route, Handler)] -> Request -> IO Response
route [] req =
  pure $ responseLBS status404 [("Content-Type", "text/html")] "Not found"
route (h : hs) req | isCorrectRoute currentRoute path method = snd h req
                   | otherwise                               = route hs req
 where
  currentRoute = fst h
  path         = pathInfo req
  method       = requestMethod req

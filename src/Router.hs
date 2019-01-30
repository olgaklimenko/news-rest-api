{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Data.ByteString
import           Data.Text
import           Network.Wai
import           Network.HTTP.Types
import           Handlers.Handlers
import           Handlers.User
import           Handlers.Author
import           Handlers.Tag
import           Handlers.Category
import           Handlers.News
import           Middlewares
import           Queries.Tag

data Route = PathRoute Text Route | DynamicRoute Text Route | MethodRoute ByteString

createAuthorRoute :: Route
createAuthorRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "POST"

getAuthorsListRoute :: Route
getAuthorsListRoute = PathRoute "api" $ PathRoute "authors" $ MethodRoute "GET"

createUserRoute :: Route
createUserRoute = PathRoute "api" $ PathRoute "users" $ MethodRoute "POST"

updateUserRoute :: Route
updateUserRoute =
  PathRoute "api" $ PathRoute "users" $ DynamicRoute "pk" $ MethodRoute "PATCH"

createTagRoute :: Route
createTagRoute = PathRoute "api" $ PathRoute "tags" $ MethodRoute "POST"

createCategoryRoute :: Route
createCategoryRoute =
  PathRoute "api" $ PathRoute "categories" $ MethodRoute "POST"

getCategoriesListRoute :: Route
getCategoriesListRoute =
  PathRoute "api" $ PathRoute "categories" $ MethodRoute "GET"

getCategoryWithParentsRoute :: Route
getCategoryWithParentsRoute =
  PathRoute "api" $ PathRoute "categories" $ DynamicRoute "pk" $ MethodRoute
    "GET"

updateCategoryRoute :: Route
updateCategoryRoute =
  PathRoute "api" $ PathRoute "categories" $ DynamicRoute "pk" $ MethodRoute
    "PATCH"

createNewsRoute :: Route
createNewsRoute = PathRoute "api" $ PathRoute "news" $ MethodRoute "POST"

routes :: [(Route, Handler)]
routes =
  [ (createAuthorRoute          , createAuthorHandler)
  , (getAuthorsListRoute        , getAuthorsListHandler)
  , (createUserRoute            , createUserHandler)
  , (updateUserRoute            , updateUserHandler)
  -- , (createTagRoute, checkPermission (Owner isOwnerOfTag) createTagHandler)
  , (createTagRoute             , createTagHandler)
  , (createCategoryRoute        , createCategoryHandler)
  , (getCategoriesListRoute     , getCategoriesListHandler)
  , (getCategoryWithParentsRoute, getCategoryWithParentsHandler)
  , (updateCategoryRoute        , updateCategoryHandler)
  , (createNewsRoute            , createNewsHandler)
  , ( MethodRoute "GET"
    , const $ pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"
    )
  ]

isCorrectRoute :: Route -> [Text] -> ByteString -> Bool
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

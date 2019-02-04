{-# LANGUAGE OverloadedStrings #-}

module Routes.Category where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T

import           Server.Routes
import           Server.Handlers
import           Handlers.Category
import           Serializers.Category
import           Data.Proxy
import           Models.Category

categoryRoutes :: [(Route, Handler)]
categoryRoutes =
    [ (createCategoryRoute        , createCategoryHandler)
    , (getCategoriesListRoute     , list categoryToResponse)
    , (getCategoryWithParentsRoute, getCategoryWithParentsHandler)
    , (updateCategoryRoute        , updateCategoryHandler)
    , (deleteCategoryRoute        , remove (Proxy :: Proxy Category))
    ]

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

deleteCategoryRoute :: Route
deleteCategoryRoute =
    PathRoute "api" $ PathRoute "categories" $ DynamicRoute "pk" $ MethodRoute
        "DELETE"

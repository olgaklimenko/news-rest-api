{-# LANGUAGE OverloadedStrings #-}

module Routes.Category where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C

import           Server.Routes
import           Server.Handlers
import           Handlers.Category

categoryRoutes :: C.Config -> [(Route, Handler)]
categoryRoutes conf =
    [ (createCategoryRoute        , createCategoryHandler conf)
    , (getCategoriesListRoute     , getCategoriesListHandler conf)
    , (getCategoryWithParentsRoute, getCategoryWithParentsHandler conf)
    , (updateCategoryRoute        , updateCategoryHandler conf)
    , (deleteCategoryRoute        , deleteCategoryHandler conf)
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


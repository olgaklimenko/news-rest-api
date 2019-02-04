{-# LANGUAGE OverloadedStrings #-}
module Handlers.Category where

import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.ByteString.Internal      as BS
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Database
import           Server.Handlers
import           Server.Helpers
import           Queries.Category
import           Models.Category
import           Serializers.Category
import           Data.Maybe
import           Server.Config
import           Server.Pagination


createCategoryHandler :: Handler
createCategoryHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    body <- liftIO $ requestBody req
    let categoryData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateCategoryRequest
    either (pure . reportParseError) (addCategory conn) categoryData
  where
    addCategory conn categoryData = do
        category <- liftIO $ createCategory conn $ requestToCategory
            categoryData
        let categoryJSON = encode $ categoryToResponse category
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           categoryJSON

getCategoryWithParentsHandler :: Handler
getCategoryWithParentsHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    either invalidIdResponse
           (successResponse conn)
           (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse conn categoryId = do
        categoriesList <- liftIO $ getCategoryWithParents conn (Just categoryId)
        let nestedCategoriesJson = encode $ categoriesToNestedCategoryResponse
                (head categoriesList)
                (tail categoriesList)
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           nestedCategoriesJson

    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

updateCategoryHandler :: Handler
updateCategoryHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    either invalidIdResponse
           (successResponse req conn)
           (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)

  where
    successResponse req conn categoryId = do
        body <- liftIO $ requestBody req
        let categoryData =
                eitherDecode $ LB.fromStrict body :: Either
                        String
                        UpdateCategoryRequest
        either (pure . reportParseError)
               (updateCategoryFields conn)
               categoryData
      where
        updateCategoryFields conn categoryData = do
            category <-
                liftIO
                $ updateCategory conn categoryId
                $ requestToUpdateCategory categoryData
            let categoryJSON = encode $ categoryToResponse category
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               categoryJSON


deleteCategoryHandler :: Handler
deleteCategoryHandler = do
    conn <- asks hConn
    req  <- asks hRequest
    either invalidIdResponse
           (successResponse conn)
           (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse conn categoryId = do
        deleted <- liftIO $ deleteCategory conn categoryId
        case deleted of
            0 -> notFoundResponse
            _ -> pure $ responseLBS status204
                                    [("Content-Type", "application/json")]
                                    ""

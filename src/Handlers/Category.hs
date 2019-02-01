{-# LANGUAGE OverloadedStrings #-}
module Handlers.Category where

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

getCategoriesListHandler :: C.Config -> Handler
getCategoriesListHandler conf req = do
    maxLimit <- Limit <$> getConf conf "pagination.max_limit"
    let pagination = getLimitOffset maxLimit req
    categories <- getCategoriesList conf pagination
    let categoriesJson = encode $ categoryToResponse <$> categories

    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       categoriesJson

getCategoryIdFromUrl :: [T.Text] -> Either String T.Text
getCategoryIdFromUrl ["api", "categories", categoryId] = Right categoryId
getCategoryIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

createCategoryHandler :: C.Config -> Handler
createCategoryHandler conf req = do
    body <- requestBody req
    let categoryData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateCategoryRequest
    either (pure . reportParseError) addCategory categoryData
  where
    addCategory categoryData = do
        category <- createCategory conf $ requestToCategory categoryData
        let categoryJSON = encode $ categoryToResponse category
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           categoryJSON

getCategoryWithParentsHandler :: C.Config -> Handler
getCategoryWithParentsHandler conf req = either
    invalidIdResponse
    successResponse
    (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse categoryId = do
        categoriesList <- getCategoryWithParents conf (Just categoryId)
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

updateCategoryHandler :: C.Config -> Handler
updateCategoryHandler conf req = either
    invalidIdResponse
    successResponse
    (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse categoryId = do
        body <- requestBody req
        let categoryData =
                eitherDecode $ LB.fromStrict body :: Either
                        String
                        UpdateCategoryRequest
        either (pure . reportParseError) updateCategoryFields categoryData
      where
        updateCategoryFields categoryData = do
            category <- updateCategory conf categoryId
                $ requestToUpdateCategory categoryData
            let categoryJSON = encode $ categoryToResponse category
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               categoryJSON
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

deleteCategoryHandler :: C.Config -> Handler
deleteCategoryHandler conf req = either
    invalidIdResponse
    successResponse
    (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse categoryId = do
        deleted <- deleteCategory conf categoryId
        case deleted of
            0 -> notFoundResponse
            _ -> pure $ responseLBS status204
                                    [("Content-Type", "application/json")]
                                    ""
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

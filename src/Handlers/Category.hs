{-# LANGUAGE OverloadedStrings #-}
module Handlers.Category where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Data.Aeson
import           Queries.Category
import           Models.Category
import           Serializers.Category
import           Helpers
import           Handlers.Handlers
import qualified Data.ByteString.Internal as BS

getCategoriesListHandler :: Handler
getCategoriesListHandler req = do
    categories <- getCategoriesList
    let categoriesJson = encode $ categoryToResponse <$> categories

    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       categoriesJson

getCategoryIdFromUrl :: [T.Text] -> Either String T.Text
getCategoryIdFromUrl ["api", "categories", categoryId] = Right categoryId
getCategoryIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

createCategoryHandler :: Handler
createCategoryHandler req = do
    body <- requestBody req
    let categoryData =
            eitherDecode $ LB.fromStrict body :: Either
                    String
                    CreateCategoryRequest
    either (pure . reportParseError) addCategory categoryData
  where
    addCategory categoryData = do
        category <- createCategory $ requestToCategory categoryData
        let categoryJSON = encode $ categoryToResponse category
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           categoryJSON

getCategoryWithParentsHandler :: Handler
getCategoryWithParentsHandler req = either
    invalidIdResponse
    successResponse
    (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse categoryId = do
        categoriesList <- getCategoryWithParents (Just categoryId)
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
updateCategoryHandler req = either
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
            category <- updateCategory categoryId
                $ requestToUpdateCategory categoryData
            let categoryJSON = encode $ categoryToResponse category
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               categoryJSON
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

deleteCategoryHandler :: Handler
deleteCategoryHandler req = either
    invalidIdResponse
    successResponse
    (getCategoryIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse categoryId = do
            deleted <- deleteCategory categoryId
            case deleted of
                0 -> notFoundResponse
                _ -> pure $ responseLBS status204
                               [("Content-Type", "application/json")]
                               ""
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

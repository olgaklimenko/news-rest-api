{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Serializers.Category where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Models.Category
import           Data.Functor.Identity

data CategoryRequestT f = CategoryRequestT {
  categoryRequestName :: f T.Text,
  categoryRequestParentId :: f (Maybe Integer)
}

newtype CreateCategoryRequest = CreateCategoryRequest (CategoryRequestT Identity)

-- -- with newtype
-- example = CreateCategoryRequest
--   $ CategoryRequestT (Identity "name") (Identity $ Just 12)

-- -- with type
-- example2 = CategoryRequestT (Identity "name") (Identity $ Just 12)

-- type CreateCatRequest = (CategoryRequestT Identity)

-- instance FromJSON (CategoryRequestT Identity) where
--   parseJSON = withObject "CreateCatRequest" $ \v ->
--     CategoryRequestT
--       <$> (Identity <$> (v .: "name"))
--       <*> (Identity <$> (v .: "parent_id"))


instance FromJSON CreateCategoryRequest where
  parseJSON = withObject "CreateCategoryRequest" $ \v ->
    fmap CreateCategoryRequest
      $   CategoryRequestT
      <$> (Identity <$> (v .: "name"))
      <*> (Identity <$> (v .:? "parent_id"))

newtype CreateCategoryResponse = CreateCategoryResponse Category

instance ToJSON CreateCategoryResponse where
  toJSON (CreateCategoryResponse Category {..}) = object
    [ "category_id" .= categoryId
    , "name" .= categoryName
    , "parent_id" .= categoryParentId
    ]

requestToCategory :: CreateCategoryRequest -> CategoryRaw
requestToCategory (CreateCategoryRequest CategoryRequestT {..}) = CategoryRaw
  (runIdentity categoryRequestName)
  (runIdentity categoryRequestParentId)

categoryToResponse :: Category -> CreateCategoryResponse
categoryToResponse = CreateCategoryResponse

data NestedCategoryResponse = NestedCategoryResponse {
  ncrId :: Integer,
  ncrName :: T.Text,
  ncrParentCategory :: NestedCategoryResponse
} | CategoryResponse {
  crId :: Integer,
  crName :: T.Text
}

instance ToJSON NestedCategoryResponse where
  toJSON NestedCategoryResponse {..} = object
    [ "category_id" .= ncrId
    , "name" .= ncrName
    , "parent_category" .= ncrParentCategory
    ]
  toJSON CategoryResponse {..} =
    object ["category_id" .= crId, "name" .= crName]

categoriesToNestedCategoryResponse
  :: Category -> [Category] -> NestedCategoryResponse
categoriesToNestedCategoryResponse current [] =
  CategoryResponse { crId = categoryId current, crName = categoryName current }
categoriesToNestedCategoryResponse current (x1 : xs) = NestedCategoryResponse
  { ncrId             = categoryId current
  , ncrName           = categoryName current
  , ncrParentCategory = categoriesToNestedCategoryResponse x1 xs
  }

newtype UpdateCategoryRequest = UpdateCategoryRequest (CategoryRequestT Maybe)

instance FromJSON UpdateCategoryRequest where
  parseJSON = withObject "UpdateCategoryRequest" $ \v ->
    fmap UpdateCategoryRequest
      $   CategoryRequestT
      <$> v
      .:? "name"
      <*> v
      .:? "parent_id"

requestToUpdateCategory :: UpdateCategoryRequest -> CategoryRawPartial
requestToUpdateCategory (UpdateCategoryRequest CategoryRequestT { categoryRequestName = name, categoryRequestParentId = Just pId })
  = CategoryRawPartial { crpName = name, crpParentId = pId }
requestToUpdateCategory (UpdateCategoryRequest CategoryRequestT { categoryRequestName = name, categoryRequestParentId = Nothing })
  = CategoryRawPartial { crpName = name, crpParentId = Nothing }
-- TODO: Use join from monad

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Queries.Category where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.Category
import           Database
import           Data.String

createCategory :: CategoryRaw -> IO Category
createCategory category = bracket (connect connectInfo) close $ \conn -> do
  (category : _) <- query conn (insertCategoryQuery category) ()
  pure category


insertCategoryQuery :: CategoryRaw -> Query
insertCategoryQuery CategoryRaw {..} =
  let
    toQuery  = fromString . T.unpack
    nameFieldExpr = "name"
    parentIdFieldExpr = maybe "" (const "parent_id") categoryRawParentId

    nameValueExpr = "'" <> categoryRawName <> "'"
    parentIdValueExpr = maybe "" show categoryRawParentId

    fields = 
        toQuery
        . T.intercalate ","
        . filter (not . T.null)
        $ [nameFieldExpr, T.pack parentIdFieldExpr]

    values = 
        toQuery
        . T.intercalate ","
        . filter (not . T.null)
        $ [nameValueExpr, T.pack parentIdValueExpr]
  in
    "INSERT INTO categories ("
    <> fields
    <> ") VALUES ("
    <> values
    <> ") RETURNING category_id, name, parent_id"


getCategoriesList :: IO [Category]
getCategoriesList = bracket (connect connectInfo) close $ \conn ->
  query conn selectQuery ()
  where
    selectQuery = 
      "SELECT * FROM categories;"


getCategoryWithParents ::  Maybe Integer -> IO [Category]
getCategoryWithParents Nothing = pure []
getCategoryWithParents pId = reverse <$> go [] pId
  where
    go acc Nothing = pure acc
    go acc (Just pId) = do
      pCat <- getCategory pId
      go (pCat : acc) (categoryParentId pCat)

getCategory :: Integer -> IO Category
getCategory categoryId = bracket (connect connectInfo) close $ \conn -> do
  (category : _) <- query conn selectQuery [categoryId]
  pure category
  where
    selectQuery = 
      "SELECT * FROM categories where category_id = ?;"

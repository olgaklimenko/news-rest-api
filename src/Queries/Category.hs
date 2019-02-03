{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Queries.Category where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Models.Category
import           Server.Database
import           Data.String
import           Server.Helpers
import           GHC.Int
import           Server.Pagination

createCategory :: Connection -> CategoryRaw -> IO Category
createCategory conn category = do
  (category : _) <- query conn (insertCategoryQuery category) ()
  pure category


insertCategoryQuery :: CategoryRaw -> Query
insertCategoryQuery CategoryRaw {..} =
  let toQuery           = fromString . T.unpack
      nameFieldExpr     = "name"
      parentIdFieldExpr = maybe "" (const "parent_id") categoryRawParentId

      nameValueExpr     = "'" <> categoryRawName <> "'"
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
  in  "INSERT INTO categories ("
        <> fields
        <> ") VALUES ("
        <> values
        <> ") RETURNING category_id, name, parent_id"

getCategoryWithParents :: C.Config -> Maybe Integer -> IO [Category]
getCategoryWithParents conf Nothing = pure []
getCategoryWithParents conf pId     = reverse <$> go [] pId
 where
  go acc Nothing    = pure acc
  go acc (Just pId) = do
    pCat <- getCategory conn pId
    go (pCat : acc) (categoryParentId pCat)

getCategory :: Connection -> Integer -> IO Category
getCategory conn categoryId = do
  (category : _) <- query conn selectQuery [categoryId]
  pure category
  where selectQuery = "SELECT * FROM categories where category_id = ?;"

updateCategory :: Connection -> Integer -> CategoryRawPartial -> IO Category
updateCategory conn cId category = do
  let log = updateCategoryQuery cId category
  print log
  res <- query conn (updateCategoryQuery cId category) ()
  pure $ head res

updateCategoryQuery :: Integer -> CategoryRawPartial -> Query
updateCategoryQuery cId CategoryRawPartial {..} =
  let toQuery  = fromString . T.unpack
      nameExpr = maybe "" (\name -> "name = '" <> name <> "'") crpName
      parentIdExpr =
          maybe "" (\pId -> "parent_id = '" <> show pId <> "'") crpParentId
      params =
          toQuery
            . T.intercalate ","
            . filter (not . T.null)
            $ [nameExpr, T.pack parentIdExpr]
  in  "UPDATE categories SET "
        <> params
        <> " WHERE category_id="
        <> toQuery (T.pack $ show cId)
        <> " RETURNING category_id, name, parent_id"

deleteCategory :: Connection -> Integer -> IO GHC.Int.Int64
deleteCategory conn cId = execute conn deleteQuery [cId]
  where deleteQuery = "DELETE FROM categories WHERE category_id=?"

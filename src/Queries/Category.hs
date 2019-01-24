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

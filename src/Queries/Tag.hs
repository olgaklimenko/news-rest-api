{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Queries.Tag where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.Tag
import           Database

-- addTagToDB :: TagRaw -> IO Tag
-- addTagToDB TagRaw {..} = bracket (connect connectInfo) close $ \conn -> do
--   (tag : _) <- query conn insertTagQuery (tagRawName)
--   pure tag

insertTagQuery :: Query
insertTagQuery =
  "INSERT INTO tags(tag_id, name) VALUES (default,?) RETURNING tag_id, name"

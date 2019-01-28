{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Queries.Tag where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.Tag
import           Database

createTag :: TagRaw -> IO Tag
createTag TagRaw {..} = bracket (connect connectInfo) close $ \conn -> do
  (tag : _) <- query conn insertTagQuery [tagRawName] :: IO [Tag]
  pure tag
  where insertTagQuery = "INSERT INTO tags(tag_id, name) VALUES (default,?) RETURNING tag_id, name"

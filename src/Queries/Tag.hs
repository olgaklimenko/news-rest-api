{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Queries.Tag where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.Tag
import           Models.User
import           Server.Database
import           Server.Helpers
import           GHC.Int

createTag :: Connection -> TagRaw -> IO Tag
createTag conn TagRaw {..} = do
  (tag : _) <- query conn insertTagQuery [tagRawName] :: IO [Tag]
  pure tag
 where
  insertTagQuery =
    "INSERT INTO tags(tag_id, name) VALUES (default,?) RETURNING tag_id, name"

selectTagsFilteredByIdQuery :: [Integer] -> Query
selectTagsFilteredByIdQuery tIds =
  "SELECT * FROM tags where tag_id in (" <> values <> ")"
 where
  values = textToQuery $ idsToText "" ((T.pack . show) <$> tIds)
  idsToText t []       = t
  idsToText t (x : []) = idsToText (t <> x) []
  idsToText t (x : xs) = idsToText (t <> x <> ",") xs

getTagById :: Connection -> Integer -> IO (Maybe Tag)
getTagById conn uid = do
  tag <- query conn q [uid]
  case tag of
    []        -> pure Nothing
    (tag : _) -> pure (Just tag)
  where q = "SELECT * FROM tags WHERE tag_id=?"

isOwnerOfTag :: User -> Integer -> IO Bool
isOwnerOfTag _ _ = pure False

updateTag :: Connection -> Integer -> TagRaw -> IO Tag
updateTag conn tId TagRaw {..} = do
  (tag : _) <- query conn updateTagQuery (tagRawName, tId)
  pure tag
 where
  updateTagQuery =
    "UPDATE tags SET name=? WHERE tag_id=? RETURNING tag_id, name"


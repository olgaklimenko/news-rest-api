{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries.News where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.News
import           Models.Tag
import           Queries.Tag

import           Database
import           Data.String
import           Helpers

insertNews :: (NewsRaw, NewsTagsRaw) -> IO (News, [Tag])
insertNews (NewsRaw {..}, newsTagRaw) =
    bracket (connect connectInfo) close $ \conn -> do
        let tagIds = ntrTagIds newsTagRaw
        (news : _) <- query
            conn
            insertNewsQuery
            ( newsRawTitle
            , newsRawAuthorId
            , newsRawCategoryId
            , newsRawContent
            , newsRawMainPhoto
            )
        newsTags <- query conn (insertNewsTagsQuery (newsId news) tagIds) () :: IO [NewsTag]
        tags <- query conn (selectTagsFilteredByIdQuery tagIds) ()
        pure (news, tags)

insertNewsQuery :: Query
insertNewsQuery =
    "INSERT INTO news(news_id, title, date_created, author_id, category_id, content, main_photo) VALUES (default,?,CURRENT_TIMESTAMP,?,?,?,?) RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

insertNewsTagsQuery :: Integer -> [Integer] -> Query
insertNewsTagsQuery nId tIds =
    "INSERT INTO tags_news(tag_id, news_id) VALUES"
        <> values
        <> "RETURNING tag_id, news_id"
  where
    values  = textToQuery $ idsToText "" (integerToText <$> tIds)
    nIdText = integerToText nId
    idsToText :: T.Text -> [T.Text] -> T.Text
    idsToText t [] = t
    idsToText t (x : []) =
        idsToText (t <> "(" <> x <> "," <> nIdText <> ")") []
    idsToText t (x : xs) =
        idsToText (t <> "(" <> x <> "," <> nIdText <> "),") xs

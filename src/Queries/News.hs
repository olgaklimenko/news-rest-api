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
insertNews (NewsRaw {..}, newsTagRaw) = bracket (connect connectInfo) close $ \conn -> do
    (news : _) <- query
        conn
        insertNewsQuery
        ( newsRawTitle
        , newsRawAuthorId
        , newsRawCategoryId
        , newsRawContent
        , newsRawMainPhoto
        )
    -- (tagsNews : _) <- query
    --     conn
    --     (insertNewsTagsQuery (newsId news) (ntrTagIds newsTagRaw))
    --     ()   
    tags <- selectTagsFilteredById (ntrTagIds newsTagRaw)
    pure (news, tags)

insertNewsQuery :: Query
insertNewsQuery = "INSERT INTO news(news_id, title, date_created, author_id, category_id, content, main_photo) VALUES (default,?,CURRENT_TIMESTAMP,?,?,?,?) RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"

-- INSERT INTO public."Item" ("Id", name)
-- VALUES  ('1', 'name1'),
--         ('2', 'name2'),
--         ('3','name3')

-- insertNewsTagsQuery :: Integer -> [Integer] -> Query
-- insertNewsTagsQuery nId tags = "INSERT INTO tags_news(tag_id, news_id) VALUES <> values <> RETURNING news_id, title, date_created, author_id, category_id, content, main_photo, is_draft"
--     where 
--         values = textToQuery $ idsToText "" ((T.pack . show) <$> tags)
--         idsToText t []       = t
--         idsToText t (x : []) = idsToText (t <> "(" <> x <> "," <> nId <> ")") []
--         idsToText t (x : xs) = idsToText (t <> x <> ",") xs


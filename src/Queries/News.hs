{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Queries.News where
import           Database.PostgreSQL.Simple
import           Control.Exception
import qualified Data.Text                     as T
import           Models.News
import           Models.Tag
import           Models.Category
import           Models.User
import           Models.Author
import           Queries.Tag
import           Queries.Category
import           Queries.Author
import           Server.Database
import           Data.String
import           Server.Helpers
import           Data.List                      ( (\\) )

createNews
    :: Connection
    -> (NewsRaw, NewsTagsRaw)
    -> IO (News, [Tag], [Category], (User, Author))
createNews conn (NewsRaw {..}, newsTagRaw) = do
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
    newsTags <-
        query conn (insertNewsTagsQuery (newsId news) tagIds) () :: IO [NewsTag]
    tags           <- query conn (selectTagsFilteredByIdQuery tagIds) ()
    categories     <- getCategoryWithParents conn (Just $ newsCategoryId news)
    userWithAuthor <- getAuthorById conn (newsAuthorId news)
    pure (news, tags, categories, userWithAuthor)

updateNews
    :: Connection
    -> Integer
    -> (NewsRawPartial, NewsTagsPartialRaw)
    -> IO (News, [Tag], [Category], (User, Author))
updateNews conn nId (news, tags) = do
    let tagIds = ntrpTagIds tags
    (news : _)         <- query conn (updateNewsQuery nId news) ()
    tagsIdsAfterUpdate <-
        (case tagIds of
            Nothing   -> query conn listTagsNewsByNewsIdQuery [nId]
            Just tIds -> renewNewsTagsRelations conn nId tIds
        )

    tags <- query
        conn
        (selectTagsFilteredByIdQuery (ntTagId <$> tagsIdsAfterUpdate))
        ()

    categories     <- getCategoryWithParents conn (Just $ newsCategoryId news)
    userWithAuthor <- getAuthorById conn (newsAuthorId news)
    pure (news, tags, categories, userWithAuthor)

renewNewsTagsRelations :: Connection -> Integer -> [Integer] -> IO [NewsTag]
renewNewsTagsRelations conn nId tagsIds = do
    existentRelations <- query conn listTagsNewsByNewsIdQuery [nId]
    let 
        existentTagsIds = ntTagId <$> existentRelations
        rForCreate = (\\) tagsIds existentTagsIds
        rForDelete = (\\) existentTagsIds tagsIds

    deleted            <- delRel rForDelete
    created            <- createRel rForCreate :: IO [NewsTag]
    tagsIdsAfterUpdate <- query conn listTagsNewsByNewsIdQuery [nId]
    pure tagsIdsAfterUpdate
  where
    delRel l = execute conn (deleteNewsTagsQuery l) ()
    createRel l = query conn (insertNewsTagsQuery nId l) ()

listTagsNewsByNewsIdQuery :: Query
listTagsNewsByNewsIdQuery =
    "SELECT * FROM tags_news WHERE news_id=? RETURNING tag_id"

insertNewsQuery :: Query
insertNewsQuery =
    "INSERT INTO news(news_id, title, date_created, author_id, category_id, \
    \content, main_photo) VALUES (default,?,CURRENT_TIMESTAMP,?,?,?, ?) \
    \RETURNING news_id, title, date_created, author_id, category_id, content, main_photo"

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

deleteNewsTagsQuery :: [Integer] -> Query
deleteNewsTagsQuery tIds =
    "DELETE FROM tags_news WHERE tag_id in (" <> values <> ")"
  where
    values = textToQuery $ idsToText "" (integerToText <$> tIds)
    idsToText :: T.Text -> [T.Text] -> T.Text
    idsToText t []       = t
    idsToText t (x : []) = idsToText (t <> x) []
    idsToText t (x : xs) = idsToText (t <> x <> ",") xs

updateNewsQuery :: Integer -> NewsRawPartial -> Query
updateNewsQuery nId NewsRawPartial {..} =
    let
        toQuery      = fromString . T.unpack
        titleExpr    = maybe "" (\x -> "title = '" <> x <> "'") nrpTitle
        categoryExpr = maybe ""
                             (\x -> "category_id = '" <> show x <> "'")
                             nrpCategoryId
        contentExpr = maybe "" (\x -> "content = '" <> x <> "'") nrpContent
        photoExpr   = maybe "" (\x -> "main_photo = '" <> x <> "'") nrpMainPhoto
        params =
            toQuery
                . T.intercalate ","
                . filter (not . T.null)
                $ [titleExpr, T.pack categoryExpr, contentExpr, photoExpr]
    in
        "UPDATE news SET "
        <> params
        <> "WHERE news_id="
        <> toQuery (integerToText nId)
        <> "RETURNING news_id, title, date_created, author_id,\
            \ category_id, content, main_photo"

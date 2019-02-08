{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.News where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T
import           Data.Time
import           Server.Handlers
import           Server.Database
import           Models.Category                ( Category )
import           Models.Tag                     ( Tag )
import           Models.Author                  ( Author )
import           Models.User                    ( User )
import           Data.Proxy
import           Server.Pagination              ( Limit
                                                , Offset
                                                )
import           Server.Helpers                 ( inductiveTupleToTuple )

data News = News {
  newsId :: Integer,
  newsTitle :: T.Text,
  newsDateCreated :: LocalTime,
  newsAuthorId :: Integer,
  newsCategoryId :: Integer,
  newsContent :: T.Text,
  newsMainPhoto :: T.Text
} deriving Show

instance FromRow News where
  fromRow =
    News <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow News where
  toRow News {..} =
    [ toField newsId
    , toField newsTitle
    , toField newsDateCreated
    , toField newsAuthorId
    , toField newsCategoryId
    , toField newsContent
    , toField newsMainPhoto
    ]

data NewsRaw = NewsRaw {
  newsRawTitle :: T.Text,
  newsRawAuthorId :: Integer,
  newsRawCategoryId :: Integer,
  newsRawContent :: T.Text,
  newsRawMainPhoto :: T.Text
}

data NewsRawPartial = NewsRawPartial {
  nrpTitle :: Maybe T.Text,
  nrpCategoryId :: Maybe Integer,
  nrpContent :: Maybe T.Text,
  nrpMainPhoto :: Maybe T.Text
}

data NewsTag = NewsTag {
    ntTagId :: Integer,
    ntNewsId :: Integer
}

data NewsTagsRaw = NewsTagsRaw {
  ntrTagIds :: [Integer]
}

data NewsTagsPartialRaw = NewsTagsPartialRaw {
  ntrpTagIds :: Maybe [Integer]
}

instance FromRow NewsTag where
  fromRow = NewsTag <$> field <*> field

instance DetailRoute News where
  pathName _ = "news"

-- instance Persistent (News, [Tag], [Category], (User, Author)) where
--   tableName :: Proxy (News, [Tag], [Category], (User, Author)) -> Query
--   tableName _ = "news"

--   select :: Connection -> (Limit, Offset) -> IO [(News, [Tag], [Category], (User, Author))]
--   select conn (limit, offset) = do
--     news <- query conn newsQuery ()
    
--     pure undefined
--    where
--     newsQuery = "SELECT * FROM news LIMIT ? OFFSET ?"
--     newsComponents news = do
--       tagsIds <- query conn listTagsNewsByNewsIdQuery [newsId n]
--       tags <- query conn (selectTagsFilteredByIdQuery tagIds) ()
--       categories     <- getCategoryWithParents conn (Just $ newsCategoryId news)
--       userWithAuthor <- getAuthorById conn (newsAuthorId news)
--       pure (news, tags, categories, userWithAuthor)

--   deleteFilterField :: Proxy entity -> Query
--   deleteFilterField _ = "news_id"

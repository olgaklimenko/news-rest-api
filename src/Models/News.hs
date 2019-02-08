{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.News where

import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T
import           Data.Time
import Server.Handlers 

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
  fromRow =
    NewsTag <$> field <*> field
  
instance DetailRoute News where
  pathName _ = "news"

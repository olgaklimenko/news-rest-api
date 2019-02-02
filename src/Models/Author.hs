{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Author where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Data.Text                     as T
import           Data.Proxy

import           Server.Database
import           Server.Pagination

import           Models.User

data Author = Author {
  authorId :: Integer,
  authorUserId :: Integer,
  authorDescription :: T.Text
} deriving Show

instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field

data AuthorRaw = AuthorRaw {
  authorRawDescription :: T.Text
}

instance Persistent  (Author, User) where
  tableName :: Proxy (Author, User) -> Query
  tableName  _ = error "(Author, User) has no table"
  select :: Connection -> (Limit, Offset) -> IO [(Author, User)]
  select conn (limit, offset) =
    fmap inductiveTupleToTuple
      <$> (query_ conn authorsQuery :: IO [Author :. User])
   where
    inductiveTupleToTuple (a :. b) = (a, b)
    authorsQuery
      = "SELECT a.*, u.*  FROM authors AS a \
        \INNER JOIN users AS u \
        \ON u.user_id = a.user_id"

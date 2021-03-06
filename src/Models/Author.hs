{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module Models.Author where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text                     as T
import           Server.Database
import           Server.Pagination
import           Server.Helpers
import           Database.PostgreSQL.Simple
import           Data.Proxy
import           Models.User
import           GHC.Int
import           Server.Handlers

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

data AuthorRawPartial = AuthorRawPartial {
  apDescription :: Maybe T.Text
} 

instance Persistent (Author, User) where
  tableName :: Proxy (Author, User) -> Query
  tableName _ = "authors"

  select :: Connection -> (Limit, Offset) -> IO [(Author, User)]
  select conn (limit, offset) =
    fmap inductiveTupleToTuple
      <$> (query conn authorsQuery [unwrapLimit limit, unwrapOffset offset] :: IO
              [Author :. User]
          )
   where
    authorsQuery
      = "SELECT  a.*, u.*  FROM authors AS a \
      \INNER JOIN users AS u \
      \ON u.user_id = a.user_id \
      \LIMIT ? OFFSET ?"

  deleteFilterField :: Proxy entity -> Query
  deleteFilterField _ = "author_id"

  delete :: Proxy entity -> Connection -> Integer -> IO GHC.Int.Int64
  delete _ conn eId = execute conn deleteQuery [eId]
   where
    deleteQuery
      = "DELETE FROM users \
      \WHERE EXISTS \
      \(SELECT * FROM authors \
      \WHERE authors.user_id = users.user_id \
      \AND authors.author_id = ?)"

instance DetailRoute (Author, User) where
  pathName _ = "authors"

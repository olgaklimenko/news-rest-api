{-# LANGUAGE TypeOperators #-}

module Models.Author where

import           Database.PostgreSQL.Simple.FromRow
import           Data.Text                     as T

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

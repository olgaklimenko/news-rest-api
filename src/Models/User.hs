{-# LANGUAGE RecordWildCards #-}

module Models.User where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T
import           Data.Time

data User = User {
  userId :: Integer,
  userName :: T.Text,
  userSurname :: T.Text,
  userAvatar :: T.Text,
  userDateCreated :: LocalTime,
  userIsAdmin :: Bool
} deriving Show

data UserRaw = UserRaw {
  userRawName :: T.Text,
  userRawSurname :: T.Text,
  userRawAvatar :: T.Text
}

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow User {..} =
    [ toField userId
    , toField userName
    , toField userSurname
    , toField userAvatar
    , toField userDateCreated
    , toField userIsAdmin
    ]




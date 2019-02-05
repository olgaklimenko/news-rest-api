{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Models.User where
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T
import           Data.Time
import           Database.PostgreSQL.Simple
import           Server.Database
import           Server.Pagination
import           Server.Handlers

data User = User {
  userId :: Integer,
  userName :: T.Text,
  userSurname :: T.Text,
  userAvatar :: T.Text,
  userDateCreated :: LocalTime,
  userIsAdmin :: Bool
} deriving Show

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

data UserRaw = UserRaw {
  userRawName :: T.Text,
  userRawSurname :: T.Text,
  userRawAvatar :: T.Text
}

data UserRawPartial = UserRawPartial {
  userRawPartialName :: Maybe T.Text,
  userRawPartialSurname :: Maybe T.Text,
  userRawPartialAvatar :: Maybe T.Text
}

instance Persistent User where
  tableName _ = "users"
  deleteFilterField _ = "user_id"

instance DetailRoute User where
  pathName _ = "users"

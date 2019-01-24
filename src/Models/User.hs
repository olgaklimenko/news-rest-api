module Models.User where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T
import           Data.Time

      
data User = User {
  userId :: Integer,
  userName :: Text,
  userSurname :: Text,
  userAvatar :: Text,
  userDateCreated :: LocalTime,
  userIsAdmin :: Bool
} deriving Show

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

data UserRaw = UserRaw {
  userRawName :: Text,
  userRawSurname :: Text,
  userRawAvatar :: Text
}

data UserRawPartial = UserRawPartial {
  userRawPartialName :: Maybe Text,
  userRawPartialSurname :: Maybe Text,
  userRawPartialAvatar :: Maybe Text
}




module Models.User where

import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T
import           Data.Time


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

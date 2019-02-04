{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Models.Tag where
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T
import           Server.Database
import           Server.Pagination
import           Database.PostgreSQL.Simple

data Tag = Tag {
    tagId :: Integer,
    tagName :: T.Text
} deriving Show

data TagRaw = TagRaw {
    tagRawName :: T.Text
} deriving Show

data TagRawPartial = TagRawPartial {
    tagRawPartialName :: Maybe T.Text
} deriving Show

instance FromRow Tag where
    fromRow = Tag <$> field <*> field

instance ToRow Tag where
    toRow Tag {..} = [toField tagId, toField tagName]

instance ToRow TagRaw where
    toRow TagRaw {..} = [toField tagRawName]


instance Persistent Tag where
    tableName _ = "tags"
    deleteFilterField _ = "tag_id"

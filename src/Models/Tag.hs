{-# LANGUAGE RecordWildCards #-}

module Models.Tag where
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T

data Tag = Tag {
    tagId :: Integer,
    tagName :: T.Text
}

data TagRaw = TagRaw {
    tagRawName :: T.Text
}

instance FromRow Tag where
    fromRow = Tag <$> field <*> field

instance ToRow Tag where
    toRow Tag {..} =
        [toField tagId, toField tagName]

{-# LANGUAGE RecordWildCards #-}

module Models.Tag where
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T

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

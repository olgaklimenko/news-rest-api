{-# LANGUAGE RecordWildCards #-}

module Models.Category where
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T

data Category = Category {
    categoryId :: Integer,
    categoryName :: T.Text,
    categoryParentId :: Maybe Integer
}

data CategoryRaw = CategoryRaw {
    categoryRawName :: T.Text,
    categoryRawParentId :: Maybe Integer
}

instance FromRow Category where
    fromRow = Category <$> field <*> field <*> field

instance ToRow Category where
    toRow Category {..} =
        [toField categoryId, toField categoryName, toField categoryParentId]


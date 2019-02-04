{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Models.Category where
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T
import           Server.Database
import           Server.Pagination
import           Database.PostgreSQL.Simple
data Category = Category {
    categoryId :: Integer,
    categoryName :: T.Text,
    categoryParentId :: Maybe Integer
} deriving Show

data CategoryRaw = CategoryRaw {
    categoryRawName :: T.Text,
    categoryRawParentId :: Maybe Integer
}

data CategoryRawPartial = CategoryRawPartial {
    crpName :: Maybe T.Text,
    crpParentId :: Maybe Integer
}

instance FromRow Category where
    fromRow = Category <$> field <*> field <*> field

instance ToRow Category where
    toRow Category {..} =
        [toField categoryId, toField categoryName, toField categoryParentId]

instance Persistent Category where
    tableName _ = "categories"
    deleteFilterField _ = "category_id"

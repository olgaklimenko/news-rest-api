{-# LANGUAGE RecordWildCards #-}

module Models.Comment where
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import           Data.Text                     as T

data Comment = Comment {
    commentId :: Integer,
    commentContent :: T.Text,
    commentNewsId :: Integer
}

data CommentRaw = CommentRaw {
    commentRawContent :: T.Text,
    commentRawNewsId :: Integer
}

instance FromRow Comment where
    fromRow = Comment <$> field <*> field <*> field

instance ToRow Comment where
    toRow Comment {..} =
        [toField commentId, toField commentContent, toField commentNewsId]

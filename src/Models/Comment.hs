{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Comment where
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           Database.PostgreSQL.Simple.ToField
import qualified Data.Text                     as T
import           Server.Database                ( Persistent(..) )
import           Server.Handlers                ( DetailRoute(..) )

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

instance Persistent Comment where
    tableName _ = "commentaries"
    deleteFilterField _ = "commentary_id"

instance DetailRoute Comment where
    pathName _ = "comments"

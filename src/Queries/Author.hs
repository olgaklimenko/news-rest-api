{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Queries.Author where

import           Database.PostgreSQL.Simple
import           Data.Text                     as T
import           Control.Exception
import           Data.Time
import Models.User
import Models.Author
import Queries.User
import Database

getAuthorsList :: IO [(User, Author)]
getAuthorsList = bracket (connect connectInfo) close $ \conn ->
  fmap inductiveTupleToTuple
    <$> (query_ conn authorsQuery :: IO [User :. Author])
 where
  authorsQuery
    = "SELECT  u.*, a.*  FROM authors AS a \
    \INNER JOIN users AS u \
    \ON u.user_id = a.user_id"

inductiveTupleToTuple (u :. a) = (u, a)

addAuthorToDB :: (UserRaw, AuthorRaw) -> IO (User, Author)
addAuthorToDB (UserRaw {..}, AuthorRaw {..}) =
  bracket (connect connectInfo) close $ \conn -> do
    (user : _) <- query conn
                        insertUserQuery
                        (userRawName, userRawSurname, userRawAvatar)
    (author : _) <- query conn
                          insertAuthorQuery
                          (userId user, authorRawDescription)
    pure (user, author)

insertAuthorQuery :: Query
insertAuthorQuery =
    "INSERT INTO authors(author_id, user_id, description) VALUES (default,?,?) RETURNING author_id, user_id, description"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Queries.Author where

import           Database.PostgreSQL.Simple
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Control.Exception
import           Data.Time
import           Models.User
import           Models.Author
import           Queries.User
import           Server.Database

getAuthorsList :: C.Config -> IO [(User, Author)]
getAuthorsList conf = bracket (connectDB conf) close $ \conn ->
  fmap inductiveTupleToTuple
    <$> (query_ conn authorsQuery :: IO [User :. Author])
 where
  authorsQuery
    = "SELECT  u.*, a.*  FROM authors AS a \
    \INNER JOIN users AS u \
    \ON u.user_id = a.user_id"

inductiveTupleToTuple (u :. a) = (u, a)

addAuthorToDB :: C.Config -> (UserRaw, AuthorRaw) -> IO (User, Author)
addAuthorToDB conf (UserRaw {..}, AuthorRaw {..}) =
  bracket (connectDB conf) close $ \conn -> do
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

getAuthorById :: C.Config -> Integer -> IO (User, Author)
getAuthorById conf aId = bracket (connectDB conf) close $ \conn -> do
  (userWithAuthor : _) <-
    fmap inductiveTupleToTuple
      <$> (query conn authorsQuery [aId] :: IO [User :. Author])
  pure userWithAuthor
 where
  authorsQuery
    = "SELECT  u.*, a.*  FROM authors AS a \
      \INNER JOIN users AS u \
      \ON u.user_id = a.user_id WHERE author_id=?"

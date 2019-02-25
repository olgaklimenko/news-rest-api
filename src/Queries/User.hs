{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Queries.User where

import qualified Database.PostgreSQL.Simple    as PSQL
import           Models.User
import           Server.Database
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import           Data.String
import           GHC.Int
import           Server.Helpers                 ( integerToText )
import           Server.Pagination              ( Limit(..)
                                                , Offset(..)
                                                )


class UserRepository m where
  type Gateway m
  selectUser :: Gateway m -> (Limit, Offset) -> m [User]
  selectUserById :: Gateway m -> Integer -> m (Maybe User)
  insertUser :: Gateway m -> UserRaw -> m User
  updateUser :: Gateway m -> Integer -> UserRawPartial -> m User
  deleteUser :: Gateway m -> Integer -> m GHC.Int.Int64

instance UserRepository IO where
  type Gateway IO = PSQL.Connection
  selectUser conn (limit, offset) = PSQL.query
    conn
    selectQuery
    [unwrapLimit limit, unwrapOffset offset]
    where selectQuery = "SELECT * FROM users LIMIT ? OFFSET ?;"

  selectUserById conn uid = do

    user <- PSQL.query conn q [uid]
    case user of
      []         -> pure Nothing
      (user : _) -> pure (Just user)
    where q = "SELECT * FROM users WHERE user_id=?"

  insertUser conn UserRaw {..} = head <$> PSQL.query
    conn
    insertUserQuery
    (userRawName, userRawSurname, userRawAvatar)

  updateUser conn uid user = do
    (user : _) <- PSQL.query conn (updateUserQuery uid user) ()
    pure user

  deleteUser conn eId = PSQL.execute conn deleteQuery [eId]
    where deleteQuery = "DELETE FROM users WHERE user_id=?"

updateUserQuery :: Integer -> UserRawPartial -> PSQL.Query
updateUserQuery uid UserRawPartial {..} =
  let toQuery = fromString . T.unpack
      nameExpr =
          maybe "" (\name -> "name = '" <> name <> "'") userRawPartialName
      surnameExpr =
          maybe "" (\name -> "surname = '" <> name <> "'") userRawPartialSurname
      avatarExpr =
          maybe "" (\name -> "avatar = '" <> name <> "'") userRawPartialAvatar
      params =
          toQuery
            . T.intercalate ","
            . filter (not . T.null)
            $ [nameExpr, surnameExpr, avatarExpr]
  in  "UPDATE users SET "
        <> params
        <> "WHERE user_id="
        <> toQuery (integerToText uid)
        <> "RETURNING user_id, name, surname, avatar, date_created, is_admin"

insertUserQuery :: PSQL.Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"

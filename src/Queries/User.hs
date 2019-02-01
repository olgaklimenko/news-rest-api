{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Queries.User where

import           Database.PostgreSQL.Simple
import           Models.User
import           Server.Database
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.String
import           GHC.Int

getUsersList :: Connection -> IO [User]
getUsersList = (`getList` "users")

getUserById :: Connection -> Integer -> IO (Maybe User)
getUserById conn uid = do
  user <- query conn q [uid]
  case user of
    []         -> pure Nothing
    (user : _) -> pure (Just user)
  where q = "SELECT * FROM users WHERE user_id=?"

addUserToDB :: Connection -> UserRaw -> IO User
addUserToDB conn UserRaw {..} =
  head <$> query conn
                 insertUserQuery
                 (userRawName, userRawSurname, userRawAvatar)

updateUser :: Connection -> T.Text -> UserRawPartial -> IO User
updateUser conn uid user = do
  let qText = (updateUserQuery uid user)
  print qText
  res <- query conn qText ()
  pure $ head res

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"

updateUserQuery :: T.Text -> UserRawPartial -> Query
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
        <> toQuery uid
        <> "RETURNING user_id, name, surname, avatar, date_created, is_admin"

deleteUser :: Connection -> Integer -> IO GHC.Int.Int64
deleteUser conn cId = execute conn deleteQuery [cId]
  where deleteQuery = "DELETE FROM users WHERE user_id=?"

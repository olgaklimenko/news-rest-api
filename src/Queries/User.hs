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

getUsersList :: C.Config -> IO [User]
getUsersList = (`getList` "users")

getUserById :: C.Config -> Integer -> IO (Maybe User)
getUserById conf uid = bracket (connectDB conf) close $ \conn -> do
  user <- query conn q [uid]
  case user of
    []         -> pure Nothing
    (user : _) -> pure (Just user)
  where q = "SELECT * FROM users WHERE user_id=?"

addUserToDB :: C.Config -> UserRaw -> IO User
addUserToDB conf UserRaw {..} = bracket (connectDB conf) close $ \conn ->
  head <$> query conn
                 insertUserQuery
                 (userRawName, userRawSurname, userRawAvatar)

updateUser :: C.Config -> T.Text -> UserRawPartial -> IO User
updateUser conf uid user = bracket (connectDB conf) close $ \conn -> do
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

deleteUser :: C.Config -> Integer -> IO GHC.Int.Int64
deleteUser conf cId = bracket (connectDB conf) close
  $ \conn -> execute conn deleteQuery [cId]
  where deleteQuery = "DELETE FROM users WHERE user_id=?"

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Queries.User where

import           Database.PostgreSQL.Simple
import           Models.User
import           Server.Database
import           Control.Exception              ( bracket )
import qualified Data.Text                     as T
import           Data.String
import           GHC.Int
import Server.Helpers (integerToText)

getUserById :: Connection -> Integer -> IO (Maybe User)
getUserById conn uid = do

  user <- query conn q [uid]
  case user of
    []         -> pure Nothing
    (user : _) -> pure (Just user)
  where q = "SELECT * FROM users WHERE user_id=?"

addUserToDB :: Connection -> UserRaw -> IO User
addUserToDB conn UserRaw {..} = head
  <$> query conn insertUserQuery (userRawName, userRawSurname, userRawAvatar)

updateUser :: Connection -> Integer -> UserRawPartial -> IO User
updateUser conn uid user = do
  (user : _) <- query conn (updateUserQuery uid user) ()
  pure user

insertUserQuery :: Query
insertUserQuery =
  "INSERT INTO users(user_id, name, surname, avatar, date_created, is_admin) VALUES (default,?,?,?,CURRENT_TIMESTAMP,default) \
  \ RETURNING user_id, name, surname, avatar, date_created, is_admin"

updateUserQuery :: Integer -> UserRawPartial -> Query
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

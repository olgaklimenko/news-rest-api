{-# LANGUAGE OverloadedStrings #-}
module Handlers.Handlers where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Database
import           Serializer
import           Data.Aeson
import           Queries.Author
import           Queries.Tag
import           Queries.User
import           Queries.Category
import           Serializers.User
import           Serializers.Category
type Handler = Request -> IO Response

getAuthorsListHandler :: Handler
getAuthorsListHandler req = do
  usersAndAuthors <- getAuthorsList
  let authors          = authorToResponse <$> usersAndAuthors
      printableAuthors = encode authors
  putStrLn "Students page accessed"
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     printableAuthors

createAuthorHandler :: Handler
createAuthorHandler req = do
  body <- requestBody req
  let createAuthorData =
        eitherDecode $ LB.fromStrict body :: Either String CreateAuthorRequest
  either reportParseError createAuthor createAuthorData
 where
  createAuthor authorData = do
    (user, author) <- addAuthorToDB $ requestToAuthor authorData
    let authorJSON = encode $ authorToResponse (user, author)
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       authorJSON
  reportParseError err = pure $ responseLBS status400
                                            [("Content-Type", "plain/text")]
                                            ("Parse error: " <> BC.pack err)

createUserHandler :: Handler
createUserHandler req = do
  body <- requestBody req
  let createUserData =
        eitherDecode $ LB.fromStrict body :: Either String CreateUserRequest
  either (pure . reportParseError) createUser createUserData
 where
  createUser userData = do
    user <- addUserToDB $ requestToUser userData
    let userJSON = encode $ userToResponse user
    pure $ responseLBS status200 [("Content-Type", "application/json")] userJSON

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> BC.pack err)


createCategoryHandler :: Handler
createCategoryHandler req = do
  body <- requestBody req
  let categoryData =
        eitherDecode $ LB.fromStrict body :: Either String CreateCategoryRequest
  either (pure . reportParseError) addCategory categoryData
 where
  addCategory categoryData = do
    category <- createCategory $ requestToCategory categoryData
    let categoryJSON = encode $ categoryToResponse category
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       categoryJSON

getCategoriesListHandler :: Handler
getCategoriesListHandler req = do
  categories <- getCategoriesList
  let categoriesJson = encode $ categoryToResponse <$> categories

  putStrLn "Students page accessed"
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     categoriesJson

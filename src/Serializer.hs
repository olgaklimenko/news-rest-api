{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializer where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database
import Models.User
import Models.Author

data CreateAuthorRequest = CreateAuthorRequest {
  createAuthorRequestName :: T.Text,
  createAuthorRequestSurname :: T.Text,
  createAuthorRequestAvatar :: T.Text,
  createAuthorRequestDescription :: T.Text
}

data CreateAuthorResponse = CreateAuthorResponse {
  createAuthorResponseName :: T.Text,
  createAuthorResponseSurname :: T.Text,
  createAuthorResponseAvatar :: T.Text,
  createAuthorResponseDescription :: T.Text,
  createAuthorResponseId :: Integer,
  createAuthorResponseUserId :: Integer,
  createAuthorResponseDateCreated :: LocalTime,
  createAuthorResponseIsAdmin :: Bool
}

instance ToJSON CreateAuthorResponse where
  toJSON (CreateAuthorResponse name surname avatar desc id uid date isAdmin) =
    object
      [ "name" .= name
      , "surname" .= surname
      , "avatar" .= avatar
      , "description" .= desc
      , "author_id" .= id
      , "user_id" .= uid
      , "date_created" .= date
      , "is_admin" .= isAdmin
      ]

instance FromJSON CreateAuthorRequest where
  parseJSON = withObject "CreateAuthorRequest" $ \v ->
    CreateAuthorRequest
      <$> v
      .:  "name"
      <*> v
      .:  "surname"
      <*> v
      .:  "avatar"
      <*> v
      .:  "description"

requestToAuthor :: CreateAuthorRequest -> (UserRaw, AuthorRaw)
requestToAuthor CreateAuthorRequest {..} =
  ( UserRaw { userRawName    = createAuthorRequestName
            , userRawSurname = createAuthorRequestSurname
            , userRawAvatar  = createAuthorRequestAvatar
            }
  , AuthorRaw { authorRawDescription = createAuthorRequestDescription }
  )

authorToResponse :: (User, Author) -> CreateAuthorResponse
authorToResponse (User {..}, Author {..}) = CreateAuthorResponse
  { createAuthorResponseName        = userName
  , createAuthorResponseSurname     = userSurname
  , createAuthorResponseAvatar      = userAvatar
  , createAuthorResponseDescription = authorDescription
  , createAuthorResponseId          = authorId
  , createAuthorResponseUserId      = userId
  , createAuthorResponseDateCreated = userDateCreated
  , createAuthorResponseIsAdmin     = userIsAdmin
  }

data CreateTagRequest = CreateTagRequest {
  createTagRequestName :: T.Text 
}

data CreateTagResponse = CreateTagResponse {
  createTagResponseId :: Integer,
  createTagResponseName :: T.Text 
}

instance ToJSON CreateTagResponse where
  toJSON (CreateTagResponse tag_id name) =
    object
      [ "tag_id" .= tag_id
      , "name" .= name
      ]

instance FromJSON CreateTagRequest where
  parseJSON = withObject "CreateTagRequest" $ \v ->
    CreateTagRequest
      <$> v
      .:  "name"

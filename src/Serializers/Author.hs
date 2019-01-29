{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializers.Author where

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

data AuthorResponse = AuthorResponse {
  authorResponseName :: T.Text,
  authorResponseSurname :: T.Text,
  authorResponseAvatar :: T.Text,
  authorResponseDescription :: T.Text,
  authorResponseId :: Integer,
  authorResponseUserId :: Integer,
  authorResponseDateCreated :: LocalTime,
  authorResponseIsAdmin :: Bool
}

instance ToJSON AuthorResponse where
  toJSON (AuthorResponse name surname avatar desc id uid date isAdmin) =
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

authorToResponse :: (User, Author) -> AuthorResponse
authorToResponse (User {..}, Author {..}) = AuthorResponse
  { authorResponseName        = userName
  , authorResponseSurname     = userSurname
  , authorResponseAvatar      = userAvatar
  , authorResponseDescription = authorDescription
  , authorResponseId          = authorId
  , authorResponseUserId      = userId
  , authorResponseDateCreated = userDateCreated
  , authorResponseIsAdmin     = userIsAdmin
  }


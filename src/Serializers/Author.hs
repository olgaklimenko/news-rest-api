{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializers.Author where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Data.Functor.Identity
import           Models.User                    ( User(..)
                                                , UserRaw(..)
                                                , UserRawPartial(..)
                                                )
import           Models.Author                  ( Author(..)
                                                , AuthorRaw(..)
                                                , AuthorRawPartial(..)
                                                )

data AuthorRequestT f = AuthorRequestT {
  authorRequestName :: f T.Text,
  authorRequestSurname :: f T.Text,
  authorRequestAvatar :: f T.Text,
  authorRequestDescription :: f T.Text
}

newtype CreateAuthorRequest = CreateAuthorRequest (AuthorRequestT Identity)

instance FromJSON CreateAuthorRequest where
  parseJSON = withObject "CreateAuthorRequest" $ \v ->
    fmap CreateAuthorRequest
      $   AuthorRequestT
      <$> (Identity <$> (v .: "name"))
      <*> (Identity <$> (v .: "surname"))
      <*> (Identity <$> (v .: "avatar"))
      <*> (Identity <$> (v .: "description"))

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
  toJSON (AuthorResponse name surname avatar desc id uid date isAdmin) = object
    [ "name" .= name
    , "surname" .= surname
    , "avatar" .= avatar
    , "description" .= desc
    , "author_id" .= id
    , "user_id" .= uid
    , "date_created" .= date
    , "is_admin" .= isAdmin
    ]

data UpdateAuthorRequest = UpdateAuthorRequest (AuthorRequestT Maybe)

instance FromJSON UpdateAuthorRequest where
  parseJSON = withObject "UpdateAuthorRequest" $ \v ->
    fmap UpdateAuthorRequest
      $   AuthorRequestT
      <$> v
      .:? "name"
      <*> v
      .:? "surname"
      <*> v
      .:? "avatar"
      <*> v
      .:? "description"


requestToCreateAuthor :: CreateAuthorRequest -> (UserRaw, AuthorRaw)
requestToCreateAuthor (CreateAuthorRequest AuthorRequestT {..}) =
  ( UserRaw { userRawName    = runIdentity authorRequestName
            , userRawSurname = runIdentity authorRequestSurname
            , userRawAvatar  = runIdentity authorRequestAvatar
            }
  , AuthorRaw { authorRawDescription = runIdentity authorRequestDescription }
  )

requestToUpdateAuthor
  :: UpdateAuthorRequest -> (UserRawPartial, AuthorRawPartial)
requestToUpdateAuthor (UpdateAuthorRequest AuthorRequestT {..}) =
  ( UserRawPartial { userRawPartialName    = authorRequestName
                   , userRawPartialSurname = authorRequestSurname
                   , userRawPartialAvatar  = authorRequestAvatar
                   }
  , AuthorRawPartial { apDescription = authorRequestDescription }
  )

authorToResponse :: (Author, User) -> AuthorResponse
authorToResponse (Author {..}, User {..}) = AuthorResponse
  { authorResponseName        = userName
  , authorResponseSurname     = userSurname
  , authorResponseAvatar      = userAvatar
  , authorResponseDescription = authorDescription
  , authorResponseId          = authorId
  , authorResponseUserId      = userId
  , authorResponseDateCreated = userDateCreated
  , authorResponseIsAdmin     = userIsAdmin
  }

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Serializers.News where

import qualified Data.Text                     as T
import           Data.Time
import           Data.Functor.Identity
import           Models.News
import           Serializers.Category
import           Serializers.Author
import           Serializers.Tag
import           Models.Category
import           Models.Author
import           Models.Tag
import           Models.User
import           Data.Aeson

data NewsRequestT f = NewsRequestT {
    nrTitle :: f T.Text,
    nrAuthorId :: f Integer,
    nrCategoryId :: f Integer,
    nrContent :: f T.Text,
    nrMainPhoto :: f T.Text,
    nrTags :: f [Integer]
}

newtype CreateNewsRequest = CreateNewsRequest (NewsRequestT Identity)

instance FromJSON CreateNewsRequest where
    parseJSON = withObject "CreateNewsRequest" $ \v ->
        fmap CreateNewsRequest
            $   NewsRequestT
            <$> (Identity <$> (v .: "title"))
            <*> (Identity <$> (v .: "author"))
            <*> (Identity <$> (v .: "category"))
            <*> (Identity <$> (v .: "content"))
            <*> (Identity <$> (v .: "main_photo"))
            <*> (Identity <$> (v .: "tags"))

data  NewsResponse = NewsResponse {
    cnrTitle :: T.Text,
    cnrAuthor :: AuthorResponse,
    cnrCategory :: NestedCategoryResponse,
    cnrContent :: T.Text,
    cnrMainPhoto :: T.Text,
    cnrTags ::  [TagResponse]
}

instance ToJSON NewsResponse where
    toJSON NewsResponse {..} = object
        [ "title" .= cnrTitle
        , "author" .= cnrAuthor
        , "category" .= cnrCategory
        , "content" .= cnrContent
        , "main_photo" .= cnrMainPhoto
        , "tags" .= cnrTags
        ]


requestToNews :: CreateNewsRequest -> (NewsRaw, NewsTagsRaw)
requestToNews (CreateNewsRequest NewsRequestT {..}) =
    ( NewsRaw (runIdentity nrTitle)
              (runIdentity nrAuthorId)
              (runIdentity nrCategoryId)
              (runIdentity nrContent)
              (runIdentity nrMainPhoto)
    , NewsTagsRaw (runIdentity nrTags)
    )

newsToResponse :: (News, [Tag], [Category], (User, Author)) -> NewsResponse
newsToResponse (News {..}, tags, categories, (user,author)) = NewsResponse
    { cnrTitle     = newsTitle
    , cnrAuthor    = authorResp (author, user)
    , cnrCategory  = categoryResp categories
    , cnrContent   = newsContent
    , cnrMainPhoto = newsMainPhoto
    , cnrTags      = tagsResp tags
    }  where 
        authorResp = authorToResponse
        categoryResp (x:xs) = categoriesToNestedCategoryResponse x xs
        tagsResp tags = tagToResponse <$> tags 

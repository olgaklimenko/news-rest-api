{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Serializers.Tag where

import           Data.Aeson
import qualified Data.Text                     as T
import           Data.Time
import           Database
import           Models.Tag
import           Data.Functor.Identity


data CreateTagRequest = CreateTagRequest {
    createTagRequestName :: T.Text
}

data CreateTagResponse = CreateTagResponse {
    createTagResponseId :: Integer,
    createTagResponseName :: T.Text
}

instance FromJSON CreateTagRequest where
    parseJSON =
        withObject "CreateTagRequest" $ \v -> CreateTagRequest <$> v .: "name"

instance ToJSON CreateTagResponse where
    toJSON (CreateTagResponse id name) =
        object ["tag_id" .= id, "name" .= name]

requestToTag :: CreateTagRequest -> TagRaw
requestToTag CreateTagRequest {..} = TagRaw { tagRawName = createTagRequestName }

tagToResponse :: Tag -> CreateTagResponse
tagToResponse Tag {..} = CreateTagResponse {
    createTagResponseId = tagId,
    createTagResponseName = tagName
}

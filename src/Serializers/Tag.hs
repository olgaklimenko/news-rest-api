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


data TagRequestT f = TagRequestT {
    tagRequestName :: f T.Text
}

newtype CreateTagRequest = CreateTagRequest (TagRequestT Identity)

newtype TagResponse = TagResponse Tag

newtype UpdateTagRequest = UpdateTagRequest (TagRequestT Maybe)

instance FromJSON CreateTagRequest where
    parseJSON = withObject "CreateTagRequest" $ \v ->
        fmap CreateTagRequest $ TagRequestT <$> (Identity <$> (v .: "name"))

instance ToJSON TagResponse where
    toJSON (TagResponse Tag {..}) =
        object ["tag_id" .= tagId, "name" .= tagName]

requestToTag :: CreateTagRequest -> TagRaw
requestToTag (CreateTagRequest TagRequestT {..}) =
    TagRaw (runIdentity tagRequestName)

tagToResponse :: Tag -> TagResponse
tagToResponse = TagResponse

instance FromJSON UpdateTagRequest where
    parseJSON = withObject "UpdateTagRequest" $ \v ->
        fmap UpdateTagRequest
            $ TagRequestT
            <$> v
            .:? "name"

-- requestToUpdateTag :: UpdateTagRequest -> TagRawPartial
-- requestToUpdateTag (UpdateTagRequest TagRequestT {..}) = 
--     TagRawPartial tagRawPartialName
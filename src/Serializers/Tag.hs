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

newtype TagRequest = TagRequest (TagRequestT Identity)

newtype TagResponse = TagResponse Tag

instance FromJSON TagRequest where
    parseJSON = withObject "CreateTagRequest" $ \v ->
        fmap TagRequest $ TagRequestT <$> (Identity <$> (v .: "name"))

instance ToJSON TagResponse where
    toJSON (TagResponse Tag {..}) =
        object ["tag_id" .= tagId, "name" .= tagName]

requestToTag :: TagRequest -> TagRaw
requestToTag (TagRequest TagRequestT {..}) =
    TagRaw (runIdentity tagRequestName)

tagToResponse :: Tag -> TagResponse
tagToResponse = TagResponse

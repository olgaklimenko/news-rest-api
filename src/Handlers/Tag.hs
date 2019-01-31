{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tag where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import           Data.Aeson
import           Server.Database
import           Server.Handlers
import           Server.Helpers
import           Queries.Tag
import           Serializers.Tag


getTagIdFromUrl :: [T.Text] -> Either String T.Text
getTagIdFromUrl ["api", "tags", tagId] = Right tagId
getTagIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

createTagHandler :: Handler
createTagHandler req = do
  body <- requestBody req
  let tagData =
        eitherDecode $ LB.fromStrict body :: Either String TagRequest
  either (pure . reportParseError) addTag tagData
 where
  addTag tagData = do
    tag <- createTag $ requestToTag tagData
    let tagJSON = encode $ tagToResponse tag
    pure $ responseLBS status200 [("Content-Type", "application/json")] tagJSON

updateTagHandler :: Handler
updateTagHandler req = either
    invalidIdResponse
    successResponse
    (getTagIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse tagId = do
        body <- requestBody req
        let tagData =
                eitherDecode $ LB.fromStrict body :: Either
                        String
                        TagRequest
        either (pure . reportParseError) updateTagFields tagData
      where
        updateTagFields tagData = do
            tag <- updateTag tagId
                $ requestToTag tagData
            let tagJSON = encode $ tagToResponse tag
            pure $ responseLBS status200
                                [("Content-Type", "application/json")]
                                tagJSON
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

getTagsListHandler :: Handler
getTagsListHandler req = do
    categories <- getTagsList
    let categoriesJson = encode $ tagToResponse <$> categories

    pure $ responseLBS status200
                        [("Content-Type", "application/json")]
                        categoriesJson

deleteTagHandler :: Handler
deleteTagHandler req = either
    invalidIdResponse
    successResponse
    (getTagIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse tagId = do
            deleted <- deleteTag tagId
            case deleted of
                0 -> notFoundResponse
                _ -> pure $ responseLBS status204
                                [("Content-Type", "application/json")]
                                ""
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

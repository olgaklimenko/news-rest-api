{-# LANGUAGE OverloadedStrings #-}
module Handlers.Tag where

import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Database
import           Server.Handlers
import           Server.Helpers
import           Queries.Tag
import           Models.Tag
import           Serializers.Tag
import Server.Config
import Server.Pagination

getTagIdFromUrl :: [T.Text] -> Either String T.Text
getTagIdFromUrl ["api", "tags", tagId] = Right tagId
getTagIdFromUrl path = Left $ "incorrect_data" <> (show $ mconcat path)

createTagHandler :: C.Config -> Handler
createTagHandler conf req = do
    body <- requestBody req
    let tagData = eitherDecode $ LB.fromStrict body :: Either String TagRequest
    either (pure . reportParseError) addTag tagData
  where
    addTag tagData = do
        tag <- createTag conf (requestToTag tagData)
        let tagJSON = encode $ tagToResponse tag
        pure $ responseLBS status200
                           [("Content-Type", "application/json")]
                           tagJSON

updateTagHandler :: C.Config -> Handler
updateTagHandler conf req = either
    invalidIdResponse
    successResponse
    (getTagIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse tagId = do
        body <- requestBody req
        let tagData =
                eitherDecode $ LB.fromStrict body :: Either String TagRequest
        either (pure . reportParseError) updateTagFields tagData
      where
        updateTagFields tagData = do
            tag <- updateTag conf tagId $ requestToTag tagData
            let tagJSON = encode $ tagToResponse tag
            pure $ responseLBS status200
                               [("Content-Type", "application/json")]
                               tagJSON
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

getTagsListHandler :: C.Config -> Handler
getTagsListHandler conf req = do
    conn <- connectDB conf
    maxLimit <- Limit <$> getConf conf "pagination.max_limit"
    let pagination = getLimitOffset maxLimit req
    tags <- select conn pagination 

    let tagsJson = encode $ tagToResponse <$> tags
    pure $ responseLBS status200
                       [("Content-Type", "application/json")]
                       tagsJson

deleteTagHandler :: C.Config -> Handler
deleteTagHandler conf req = either
    invalidIdResponse
    successResponse
    (getTagIdFromUrl (pathInfo req) >>= textToInteger)
  where
    successResponse tagId = do
        deleted <- deleteTag conf tagId
        case deleted of
            0 -> notFoundResponse
            _ -> pure $ responseLBS status204
                                    [("Content-Type", "application/json")]
                                    ""
    invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

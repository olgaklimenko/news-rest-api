{-# LANGUAGE OverloadedStrings #-}
module Server.Handlers where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LB8
import qualified Data.Text                     as T
import qualified Text.Read                     as R
import qualified Data.Configurator.Types       as C
import           Data.Aeson

import           Server.Database
import           Server.Pagination
import           Server.Config


type Handler = Request -> IO Response

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> LB8.pack err)

notFoundResponse :: IO Response
notFoundResponse = pure
  $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: IO Response
hasNoPermissionResponse = notFoundResponse

list :: (Persistent a, ToJSON b) => (a -> b) -> C.Config -> Handler
list serialize conf req = do
  conn     <- connectDB conf
  maxLimit <- Limit <$> getConf conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
  entities <- select conn pagination
  let entitiesJSON = encode $ serialize <$> entities
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     entitiesJSON

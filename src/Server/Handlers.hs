{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as B8
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as LB8
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as P
import qualified Text.Read                     as R
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Pagination
import           Server.Database
import           Server.Config
import           Server.Helpers                 ( textToInteger )
import           Server.Logger
import           GHC.Int
import           Data.Proxy
import qualified Control.Logger.Simple as Log

type Handler = MonadHandler Response

data HandlerEnv = HandlerEnv {
  hConfig :: C.Config,
  hRequest :: Request,
  hConn :: P.Connection
}

newtype MonadHandler a = MonadHandler {runMonadHandler :: ReaderT HandlerEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv)

instance Logger MonadHandler where
  logDebug = liftIO . logDebug
  logInfo  = liftIO . logInfo
  logWarn  = liftIO . logWarn
  logError = liftIO . logError

class DetailRoute entity where
  pathName :: Proxy entity -> T.Text
  getIdFromUrl :: Proxy entity -> [T.Text] -> Either String T.Text
  getIdFromUrl _ ["api", pathName, eId] = Right eId
  getIdFromUrl _ path = Left $ "incorrect_data" <> show (mconcat path)

runHandler :: C.Config -> Request -> P.Connection -> MonadHandler a -> IO a
runHandler conf req conn handler = runReaderT (runMonadHandler handler) env
  where env = HandlerEnv conf req conn

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> LB8.pack err)

okResponse :: (Applicative m) => m Response
okResponse = pure $ responseLBS status200 [("Content-Type", "text/html")] "Ok"

notFoundResponse :: (Applicative m) => m Response
notFoundResponse = pure
  $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: (Applicative m) => m Response
hasNoPermissionResponse = notFoundResponse

invalidIdResponse :: (Applicative m) => String -> m Response
invalidIdResponse errorMsg = pure $ responseLBS
  status400
  [("Content-Type", "application/json")]
  ("Invalid id in url: " <> LB8.pack errorMsg)

list :: (Persistent a, ToJSON b) => (a -> b) -> Handler
list serialize = do
  req      <- asks hRequest
  conf     <- asks hConfig
  conn     <- asks hConn
  maxLimit <- liftIO $ Limit <$> getConf conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
  entities <- liftIO $ select conn pagination
  let entitiesJSON = encode $ serialize <$> entities
  pure $ responseLBS status200
                     [("Content-Type", "application/json")]
                     entitiesJSON

remove :: (Persistent entity, DetailRoute entity) => Proxy entity -> Handler
remove entityType = do
  conn <- asks hConn
  req  <- asks hRequest
  either invalidIdResponse
         (successResponse conn)
         (getIdFromUrl entityType (pathInfo req) >>= textToInteger)
 where
  successResponse conn eId = do
    logDebug $ "Delete " <> T.pack (show entityType) <> " " <> T.pack (show eId)
    deleted <- liftIO $ delete entityType conn eId
    case deleted of
      0 -> notFoundResponse
      _ ->
        pure $ responseLBS status204 [("Content-Type", "application/json")] ""

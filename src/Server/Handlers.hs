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
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import qualified Database.PostgreSQL.Simple    as P
import qualified Text.Read                     as R
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import           Server.Pagination
import           Server.Database
import           Server.Config

type Handler = MonadHandler Response

data HandlerEnv = HandlerEnv {
  hConfig :: C.Config,
  hRequest :: Request,
  hConn :: P.Connection
}

newtype MonadHandler a = MonadHandler {runMonadHandler :: ReaderT HandlerEnv IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HandlerEnv)

runHandler :: C.Config -> Request -> P.Connection -> MonadHandler a -> IO a
runHandler conf req conn handler = runReaderT (runMonadHandler handler) env
  where env = HandlerEnv conf req conn

reportParseError :: String -> Response
reportParseError err = responseLBS status400
                                   [("Content-Type", "plain/text")]
                                   ("Parse error: " <> LB8.pack err)

notFoundResponse :: (Applicative m) => m Response
notFoundResponse = pure
  $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: (Applicative m) => m Response
hasNoPermissionResponse = notFoundResponse

invalidIdResponse :: (Applicative m) => String -> m Response
invalidIdResponse errorMsg = pure $ responseLBS
        status400
        [("Content-Type", "application/json")]
        ("Invalid id in url: " <> BC.pack errorMsg)

list :: (Persistent a, ToJSON b) => (a -> b) -> C.Config -> Handler
list serialize conf req = do
  maxLimit <- Limit <$> getConf conf "pagination.max_limit"
  let pagination = getLimitOffset maxLimit req
  conn  <- connectDB conf
  users <- select conn pagination
  let usersJSON = encode $ serialize <$> users
  pure $ responseLBS status200 [("Content-Type", "application/json")] usersJSON

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Handlers where
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Data.ByteString               as B
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Data.Aeson
import qualified Database.PostgreSQL.Simple    as P

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
                                   ("Parse error: " <> BC.pack err)

notFoundResponse :: IO Response
notFoundResponse = pure
  $ responseLBS status404 [("Content-Type", "application/json")] "Not Found"

hasNoPermissionResponse :: IO Response
hasNoPermissionResponse = notFoundResponse

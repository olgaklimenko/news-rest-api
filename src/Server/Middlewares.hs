{-# LANGUAGE OverloadedStrings #-}

module Server.Middlewares where
import           Control.Monad.Reader
import           Control.Monad.IO.Class
import qualified Database.PostgreSQL.Simple    as P
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS
import qualified Data.Configurator.Types       as C
import           Text.Read
import           Network.Wai
import           Server.Handlers
import           Server.Helpers
import           Queries.User
import           Models.User

data Permission = Admin
    | Owner (User -> Integer -> IO Bool)
    | Regular

checkPermission :: Permission -> Handler -> Handler
checkPermission Admin handler = do
    conn <- asks hConn
    req <- asks hRequest
    user <- liftIO $ getUser conn req
    checkUserAdmin user handler
checkPermission (Owner f) handler  = do
    conn <- asks hConn
    req <- asks hRequest
    user <- liftIO $ getUser conn req
    checkUserOwner user f handler

checkUserAdmin :: Maybe User -> Handler -> Handler
checkUserAdmin Nothing     _     = hasNoPermissionResponse
checkUserAdmin (Just user) handler = handler 

checkUserOwner
    :: Maybe User -> (User -> Integer -> IO Bool) -> Handler -> Handler
checkUserOwner Nothing _ _ = hasNoPermissionResponse
checkUserOwner (Just user) f handler
    | userIsAdmin user = handler
    | otherwise = do
        req <- asks hRequest
        isOwner <- liftIO $ (checkOwner req)
        if isOwner then handler else hasNoPermissionResponse
  where
    checkOwner req = maybe (pure False) (f user) (objId req)
    objId req     = getObjectPk (pathInfo req)

getObjectPk :: [T.Text] -> Maybe Integer
getObjectPk ["api", _, pk] = either (const Nothing) Just (textToInteger pk)
getObjectPk path           = Nothing

getAuthHeader :: Request -> Maybe Integer
getAuthHeader req =
    let headers = requestHeaders req
        bsId    = lookup "Authorization" headers
    in  bsId >>= (readMaybe . BS.unpack)

getUser :: P.Connection -> Request -> IO (Maybe User)
getUser conn req = maybe (pure Nothing) (getUserById conn) $ getAuthHeader req

{-# LANGUAGE OverloadedStrings #-}

module Server.Middlewares where
import           Control.Monad.Reader
import           Control.Monad.IO.Class
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
checkPermission Admin handler req = do
    conn <- asks hConn
    req <- asks hRequest
    user <- liftIO $ getUser conn req
    checkUserAdmin user handler
checkPermission (Owner f) handler req = do
    conn <- asks hConn
    req <- asks hRequest
    user <- liftIO $ getUser conn req
    checkUserOwner user f handler

checkUserAdmin :: Maybe User -> Handler -> Handler
checkUserAdmin Nothing     _       _   = hasNoPermissionResponse
checkUserAdmin (Just user) handler req = handler req

checkUserOwner
    :: Maybe User -> (User -> Integer -> IO Bool) -> Handler -> Handler
checkUserOwner Nothing _ _ _ = hasNoPermissionResponse
checkUserOwner (Just user) f handler req
    | userIsAdmin user = handler req
    | otherwise = do
        isOwner <- checkOwner
        if isOwner then handler req else hasNoPermissionResponse
  where
    checkOwner = maybe (pure False) (f user) objId
    objId      = getObjectPk (pathInfo req)

getObjectPk :: [T.Text] -> Maybe Integer
getObjectPk ["api", _, pk] = either (const Nothing) Just (textToInteger pk)
getObjectPk path           = Nothing

getAuthHeader :: Request -> Maybe Integer
getAuthHeader req =
    let headers = requestHeaders req
        bsId    = lookup "Authorization" headers
    in  bsId >>= (readMaybe . BS.unpack)

getUser :: C.Config -> Request -> IO (Maybe User)
getUser conf req = maybe (pure Nothing) (getUserById conf) $ getAuthHeader req

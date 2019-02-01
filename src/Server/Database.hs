{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Server.Database where

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Migration
import           System.Directory               ( createDirectoryIfMissing )
import qualified Data.Text                     as T
import qualified Data.Configurator.Types       as C
import           Control.Exception
import           Data.Monoid                    ( (<>) )
import           GHC.Int
import           Server.Config

connectInfo :: C.Config -> IO ConnectInfo
connectInfo conf = do
  host     <- getConf conf "database.host"
  port     <- getConf conf "database.port"
  user     <- getConf conf "database.user"
  password <- getConf conf "database.password"
  database <- getConf conf "database.database"

  pure $ ConnectInfo { connectHost     = host
                     , connectPort     = port
                     , connectUser     = user
                     , connectPassword = password
                     , connectDatabase = database
                     }

connectDB :: C.Config -> IO Connection
connectDB conf = connectInfo conf >>= connect

initializeDB :: C.Config -> IO ()
initializeDB conf = do
  createDirectoryIfMissing False "./DBMigrations"
  bracket (connectDB conf) close migrate

migrate :: Connection -> IO ()
migrate conn = do
  result <- withTransaction conn (runMigrations False conn cmds)
  case result of
    MigrationError err -> error err
    _                  -> return ()
  where cmds = [MigrationInitialization, MigrationDirectory "./DBMigrations"]

getList :: FromRow a => Connection -> Query -> IO [a]
getList conn tableName = query_ conn $ "SELECT * FROM " <> tableName

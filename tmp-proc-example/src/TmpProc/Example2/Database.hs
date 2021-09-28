{-# LANGUAGE OverloadedStrings #-}

module TmpProc.Example2.Database
  ( -- * Contact Database access
    create
  , fetch
  , remove
  , migrateDB

    -- * Database location
  , Locator
  , defaultLoc
  )
where

import           Control.Monad.Logger        (LogLevel (..), LoggingT,
                                              filterLogger, runStdoutLoggingT, LogSource)
import           Control.Monad.Reader        (runReaderT)
import           Database.Persist
import           Database.Persist.Postgresql (ConnectionString, SqlPersistT,
                                              fromSqlKey, runMigration,
                                              toSqlKey, withPostgresqlConn)

import           TmpProc.Example2.Schema

{-| Specifies the database to connect to .-}
type Locator = ConnectionString


{-| A default for local development .-}
defaultLoc :: Locator
defaultLoc = "host=127.0.0.1 port=5432 contact=postgres dbname=postgres password=postgres"


migrateDB :: Locator -> IO ()
migrateDB loc = doDb loc $ runMigration migrateAll


fetch :: Locator -> ContactID -> IO (Maybe Contact)
fetch loc cid = doDb loc $ get $ toSqlKey cid


create :: Locator -> Contact -> IO ContactID
create loc contact = fromSqlKey <$> doDb loc (insert contact)


remove :: Locator -> ContactID -> IO ()
remove loc cid = doDb loc $ delete contactKey
  where
    contactKey :: Key Contact
    contactKey = toSqlKey cid


doDb :: Locator -> SqlPersistT (LoggingT IO) a -> IO a
doDb = doDb' defaultFilter


doDb' :: (LogSource -> LogLevel -> Bool) -> Locator -> SqlPersistT (LoggingT IO) a ->  IO a
doDb' logFilter loc action  =
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn loc $ \backend ->
    runReaderT action backend


defaultFilter :: a -> LogLevel -> Bool
defaultFilter _ level = level > LevelDebug

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Docker.TmpProc.Postgres
  ( -- * functions
    clearTables
  , mkTmpProc
  , mkNoResetProc

    -- * type aliases
  , TableName

  -- * re-exports
  , module System.Docker.TmpProc
  ) where


import           Control.Monad              (void)
import qualified Data.ByteString.Char8      as C8
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Database.PostgreSQL.Simple (connectPostgreSQL, execute_)

import           System.Docker.TmpProc


-- | The name of a table in the database.
type TableName = String


-- | A cleanup function that clears all rows from the specified tables.
clearTables :: [TableName] -> ProcURI -> IO ()
clearTables tables pgUri = do
  c <- connectPostgreSQL pgUri
  mapM_ (execute_ c) $ map (fromString . ((++) "DELETE FROM ")) tables


-- | A @TmpProc@ that runs the default postgresql version.
mkTmpProc :: [TableName] -> TmpProc
mkTmpProc tables = mkNoResetProc
  {
    procReset = clearTables tables
  }


-- | A @TmpProc@ that runs the default postgresql version without a configured
-- reset action.
mkNoResetProc :: TmpProc
mkNoResetProc = TmpProc
  { procImageName = pgImageName
  , procRunArgs = pgRunArgs
  , procMkUri = pgMkUri
  , procReset = doNothing
  , procPing = void . connectPostgreSQL
  }


-- | The default image to use when launching a postgres docker process.
pgImageName :: Text
pgImageName = "postgres:10.6"


-- | Makes args chosen to match the password used in 'pgMkUri'.
pgRunArgs :: [Text]
pgRunArgs =
  [ "-e"
  , "POSTGRES_PASSWORD=" <> (Text.pack $ C8.unpack $ pgDefaultPassword)
  ]


-- | Makes a uri whose password matches the one configured by ''pgRunArgs'.
pgMkUri :: DockerIpAddress -> ProcURI
pgMkUri ip = "host="
             <> C8.pack ip
             <> " dbname=postgres user=postgres password="
             <> pgDefaultPassword
             <> " port=5432"


pgDefaultPassword :: C8.ByteString
pgDefaultPassword = "mysecretpassword"

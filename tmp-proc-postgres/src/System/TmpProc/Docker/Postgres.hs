{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : System.TmpProc.Docker.Postgres
Description : Provides an instance of @Proc@ for launching postgres as a tmp process.
Copyright   : (c) 2021, Tim Emiola
License     : BSD
Maintainer  : adetokunbo@users.noreply.github.com
Stability   : experimental
-}
module System.TmpProc.Docker.Postgres
  ( -- * data types
    TmpPostgres(..)

    -- * useful definitions
  , aProc
  , aHandle

    -- * module re-exports
  , module System.TmpProc.Docker
  )
where

import           Control.Monad              (void)
import qualified Data.ByteString.Char8      as C8
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL,
                                             execute_, close)

import           System.TmpProc.Docker      (Connectable (..), HList (..),
                                             HostIpAddress, Proc (..),
                                             Proc2Handle, ProcHandle (..),
                                             SvcURI, startupAll,
                                             withTmpConn)


{-| A singleton 'HList' containing a 'TmpPostgres'. -}
aProc :: HList '[TmpPostgres]
aProc = TmpPostgres [] `HCons` HNil


{-| An 'HList' that just contains the handle created by 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[TmpPostgres]))
aHandle = startupAll aProc


{-| Represents a connection to a postgres DB running on docker.

It specifies the names of the tables to be dropped during a reset.

-}
data TmpPostgres = TmpPostgres [Text]


{-| A 'Proc' for running postgres as a tmp process. -}
instance Proc TmpPostgres where
  type Image TmpPostgres = "postgres:10.6"
  type Name TmpPostgres = "a-postgres-db"

  uriOf = mkUri'
  runArgs = runArgs'
  ping = void . connectPostgreSQL . hUri
  reset = reset'

instance Connectable TmpPostgres where
  type Conn TmpPostgres = Connection

  openConn = connectPostgreSQL . hUri
  closeConn = close


{-| Makes a uri whose password matches the one specified in 'runArgs''. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "host="
             <> C8.pack (Text.unpack ip)
             <> " dbname=postgres user=postgres password="
             <> dbPassword
             <> " port=5432"


dbPassword :: C8.ByteString
dbPassword = "mysecretpassword"


{-| Match the password used in 'mkUri''. -}
runArgs' :: [Text]
runArgs' =
  [ "-e"
  , "POSTGRES_PASSWORD=" <> Text.pack (C8.unpack dbPassword)
  ]


{-| Empty all rows in the tables, if any are specified. -}
reset' :: ProcHandle TmpPostgres -> IO ()
reset' handle@(ProcHandle {hProc}) =
  let go (TmpPostgres []) = pure ()
      go (TmpPostgres tables) = withTmpConn handle $ \c ->
        mapM_ (execute_ c . (fromString . (++) "DELETE FROM ") . Text.unpack) tables
  in go hProc

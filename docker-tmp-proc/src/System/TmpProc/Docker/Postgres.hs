{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : System.TmpProc.Docker.Postgres
Description : A specification for launching a postgres process
Copyright   : (c)
License     : BSD
Maintainer  : tim@challengehub.com
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

import           Database.PostgreSQL.Simple (connectPostgreSQL, execute_)

import           System.TmpProc.Docker            (HList (..), HostIpAddress,
                                             Proc (..), Proc2Handle,
                                             ProcHandle (..), SvcURI,
                                             startupAll)

{-| Contains just 'TmpPostgres'. -}
aProc :: HList '[TmpPostgres]
aProc = TmpPostgres [] `HCons` HNil


{-| The handle created from just 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[TmpPostgres]))
aHandle = startupAll aProc


{-| A data type representing a connection to a postgres database running on docker.

It specifies the names of the tables dropped during a reset.

-}
data TmpPostgres = TmpPostgres [Text]


{-| Specify how to run Postgres as a Tmp 'Proc'.  -}
instance Proc TmpPostgres where
  type Image TmpPostgres = "postgres:10.6"
  type Name TmpPostgres = "a-postgres-db"

  uriOf = mkUri'
  runArgs = runArgs'
  ping = void . connectPostgreSQL . hUri
  reset = reset'


{-| Makes a uri whose password matches the one specified in 'pgRunArgs'. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "host="
             <> C8.pack (Text.unpack ip)
             <> " dbname=postgres user=postgres password="
             <> dbPassword
             <> " port=5432"


dbPassword :: C8.ByteString
dbPassword = "mysecretpassword"


{-| Match the password used in 'pgMkUri'. -}
runArgs' :: [Text]
runArgs' =
  [ "-e"
  , "POSTGRES_PASSWORD=" <> Text.pack (C8.unpack dbPassword)
  ]


{-| Drop the tables if any are specified. -}
reset' :: ProcHandle TmpPostgres -> IO ()
reset' ProcHandle {hUri = uri, hProc} =
  let go (TmpPostgres []) = pure ()
      go (TmpPostgres tables) = do
        c <- connectPostgreSQL uri
        mapM_ (execute_ c . (fromString . (++) "DELETE FROM ") . Text.unpack) tables
  in go hProc

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : System.TmpProc.Docker.Postgres
Description : A specification for launching a postgres process
Copyright   : (c) 2021, Tim Emiola
License     : BSD
Maintainer  : tim.emiola@gmail.com
Stability   : experimental
-}
module System.TmpProc.Docker.Postgres
  ( -- * data types
    PgDocker(..)

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

import           System.TmpProc.Docker      (HList (..), HostIpAddress,
                                             Proc (..), Proc2Handle,
                                             ProcHandle (..), SvcURI,
                                             startupAll)


{-| A singleton 'HList' containing a 'PgDocker'. -}
aProc :: HList '[PgDocker]
aProc = PgDocker [] `HCons` HNil


{-| An 'HList' that just contains the handle created by 'aProc'. -}
aHandle :: IO (HList (Proc2Handle '[PgDocker]))
aHandle = startupAll aProc


{-| Represents a connection to a postgres DB running on docker.

It specifies the names of the tables to be dropped during a reset.

-}
data PgDocker = PgDocker [Text]


{-| Specifies how to run Postgres as a tmp 'Proc'.  -}
instance Proc PgDocker where
  type Image PgDocker = "postgres:10.6"
  type Name PgDocker = "a-postgres-db"

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
reset' :: ProcHandle PgDocker -> IO ()
reset' ProcHandle {hUri = uri, hProc} =
  let go (PgDocker []) = pure ()
      go (PgDocker tables) = do
        c <- connectPostgreSQL uri
        mapM_ (execute_ c . (fromString . (++) "DELETE FROM ") . Text.unpack) tables
  in go hProc

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Copyright   : (c) 2020-2021 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <adetokunbo@users.noreply.github.com>

An demo @HSpec@ test that use @tmp-proc@
-}
module TmpProc.Example2.IntegrationSpec where

import Control.Exception (onException)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client
  ( BaseUrl (..)
  , ClientEnv
  , Scheme (..)
  , mkClientEnv
  , runClientM
  )
import System.TmpProc.Docker.Postgres
import System.TmpProc.Docker.Redis
import Test.Hspec
import Test.Hspec.TmpProc
  ( AreProcs
  , HasHandle
  , ServerHandle
  , handleOf
  , handles
  , runServer
  , serverPort
  , shutdown
  , tdescribe
  , terminateAll
  , withConnOf
  , (&:&)
  )
import qualified TmpProc.Example2.Cache as Cache
import qualified TmpProc.Example2.Client as Client
import qualified TmpProc.Example2.Database as DB
import TmpProc.Example2.Schema (Contact (..), ContactID)
import TmpProc.Example2.Server (AppEnv (..), waiApp)


-- | The test uses a Postgres database .
dbProc :: TmpPostgres
dbProc = TmpPostgres ["contacts"] -- 'reset' will empty the contacts table


-- | The test uses Redis as a cache.
cacheProc :: TmpRedis
cacheProc = TmpRedis []


-- | Specifies the procs to be launched as test fixtures.
testProcs :: HList '[TmpPostgres, TmpRedis]
testProcs = dbProc &:& cacheProc


-- | Specifies the expected behaviour.
spec :: Spec
spec = tdescribe "Tmp.Proc:Demo of testing of DB/Cache server" $ do
  beforeAll mkFixture $ afterAll shutdown' $ do
    context "When the database is empty, using the client to fetch a contact" $ do
      it "should throw an error" $ \(_, clientEnv) ->
        fmap isLeft (runClientM (Client.fetch 1) clientEnv) `shouldReturn` True

      context "and the contact " $ do
        it "should not be found in the DB" $ \(sh, _) ->
          hasInDb sh 1 `shouldReturn` False

        it "should not be found in the cache" $ \(sh, _) -> do
          hasInCache sh 1 `shouldReturn` False

      context "and using the client to insert a contact" $ do
        it "should succeed" $ \(_, clientEnv) ->
          (fmap isLeft $ runClientM (Client.create testContact) clientEnv) `shouldReturn` False

    context "When the client is used to insert a contact" $ do
      context "then the contact " $ do
        it "should be found in the DB" $ \(sh, _) ->
          hasInDb sh 1 `shouldReturn` True

        it "should not be found in the cache" $ \(sh, _) -> do
          hasInCache sh 1 `shouldReturn` False

      context "and using the client to fetch the contact" $ do
        it "should succeed" $ \(_, clientEnv) ->
          (fmap isLeft $ runClientM (Client.fetch 1) clientEnv) `shouldReturn` False

    context "After fetching the contact with the client" $ do
      context "then the contact " $ do
        it "should be found in the cache" $ \(sh, _) -> do
          hasInCache sh 1 `shouldReturn` True


{- | Simplifies the test cases

Note the use of the 'HasHandle' constraint to indicate what TmpProcs the function uses.
-}
hasInCache :: (HasHandle TmpRedis procs) => ServerHandle procs -> ContactID -> IO Bool
hasInCache sh cid = withConnOf @TmpRedis Proxy (handles sh) $ \cache ->
  fmap isJust $ Cache.loadContact cache cid


{- | Simplifies the test cases

Here, ServerHandle specifies the full list of types required by the calling test code.
-}
hasInDb :: ServerHandle ('[TmpPostgres, TmpRedis]) -> ContactID -> IO Bool
hasInDb sh cid = do
  let dbUriOf = hUri . handleOf @"a-postgres-db" Proxy . handles
  fmap isJust $ flip DB.fetch cid $ dbUriOf sh


{- | The full test fixture.

It allows tests to

- use the servant client to invoke the backend
- check the state of service backends via the @ProcHandles@ in the 'ServerHandle'.
-}
type Fixture = (ServerHandle ('[TmpPostgres, TmpRedis]), ClientEnv)


mkFixture :: IO Fixture
mkFixture = do
  let mkApp someHandles = do
        -- handleOf can obtain a handle using either the Proc type ...
        let redisH = handleOf @TmpRedis Proxy someHandles

            -- or the Name of it's Proc type
            dbLoc = hUri $ handleOf @"a-postgres-db" Proxy someHandles

        -- Create the database schema
        DB.migrateDB dbLoc `onException` terminateAll someHandles

        -- Determine the redis location
        cache <- openConn redisH `onException` terminateAll someHandles

        pure $ waiApp $ AppEnv dbLoc cache

  sh <- runServer testProcs mkApp
  clientEnv <- clientEnvOf sh
  pure (sh, clientEnv)


shutdown' :: Fixture -> IO ()
shutdown' (sh, _) = shutdown sh


clientEnvOf :: (AreProcs procs) => ServerHandle procs -> IO ClientEnv
clientEnvOf s = do
  mgr <- newManager defaultManagerSettings
  pure $ mkClientEnv mgr $ BaseUrl Http "localhost" (serverPort s) ""


testContact :: Contact
testContact =
  Contact
    { contactName = "Bond"
    , contactEmail = "james@hmss.gov.uk"
    , contactAge = 45
    , contactTitle = "Mr"
    }

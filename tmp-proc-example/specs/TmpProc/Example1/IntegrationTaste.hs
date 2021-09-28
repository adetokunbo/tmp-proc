{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module TmpProc.Example1.IntegrationTaste where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Exception              (onException)
import qualified Data.ByteString.Char8          as C8
import           Data.Either                    (isLeft)
import           Data.Maybe                     (isJust)
import           Data.Proxy                     (Proxy (..))
import           Database.Redis                 (parseConnectInfo)
import           Network.HTTP.Client            (newManager)
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Servant.Client                 (BaseUrl (..), ClientEnv,
                                                 Scheme (..), mkClientEnv,
                                                 runClientM)

import           System.TmpProc
import           System.TmpProc.Docker.Postgres
import           System.TmpProc.Docker.Redis

import qualified TmpProc.Example1.Cache         as Cache
import qualified TmpProc.Example1.Client        as Client
import qualified TmpProc.Example1.Database      as DB
import           TmpProc.Example1.Schema        (Contact (..), ContactID)
import           TmpProc.Example1.Server        (waiApp)



{-| The test uses a Postgres database . -}
dbProc :: TmpPostgres
dbProc = TmpPostgres ["contacts"] -- 'reset' will empty the contacts table


{-| The test uses Redis as a cache. -}
cacheProc :: TmpRedis
cacheProc = TmpRedis []


{-| Specifies the procs to be launched as test fixtures.  -}
testProcs :: HList '[TmpPostgres, TmpRedis]
testProcs = dbProc &: cacheProc &: HNil


main :: IO ()
main = defaultMain $ withResource mkFixture shutdown' tests


tests :: IO Fixture -> TestTree
tests getFixture = testGroup "Tmp.Proc:Demo of testing of DB/Cache server"
  [ testGroup "When the database is empty"
    [ testCase "Using the client to fetch a contact" $ do
        (_handle, client) <- getFixture
        fetched <- fmap isLeft $ runClientM (Client.fetch 1) client
        assertBool "should succeed" fetched

    , testCase "The contact should not be found in the DB" $ do
        (handle, _client) <- getFixture
        hasInDb handle 1 >>= assertEqual "contact in DB!" False

    , testCase "The contact should not be found in the cache" $ do
        (handle, _client) <- getFixture
        hasInCache handle 1 >>= assertEqual "contact in Cache!" False
    ],

    after AllFinish "empty" $ testCase "zz: the client should insert a contact" $ do
        (_handle, client) <- getFixture
        inserted <- fmap isLeft $ runClientM (Client.create testContact) client
        assertEqual "insert failed!" False inserted


  , after AllFinish "zz" $ testGroup "After the client is inserted"
    [ testCase "the contact should be found in the database" $ do
        (handle, _client) <- getFixture
        hasInDb handle 1 >>= assertEqual "contact not in DB!" True

    , testCase "yy: the contact should not be found in the cache" $ do
        (handle, _client) <- getFixture
        hasInCache handle 1 >>= assertEqual "contact in Cache!" False

    , after AllFinish "yy" $ testCase "and the client should fetch the contact" $ do
        (_handle, client) <- getFixture
        fetched <- fmap isLeft $ runClientM (Client.fetch 1) client
        assertEqual "notFetched" False fetched
    ]

  , after AllFinish "inserted" $ testGroup "After fetching the contact"
    [ testCase "the contact should be found in the cache" $ do
        (handle, _client) <- getFixture
        hasInCache handle 1 >>= assertEqual "contact in Cache!" True
    ]
  ]


hasInCache :: ServerHandle ('[TmpPostgres, TmpRedis]) -> ContactID -> IO Bool
hasInCache sh cid = do
  cacheLoc <- cacheLocFrom $ handleOf @TmpRedis Proxy $ handles sh
  fmap isJust $ Cache.loadContact cacheLoc cid


hasInDb :: ServerHandle ('[TmpPostgres, TmpRedis]) -> ContactID -> IO Bool
hasInDb sh cid = do
  let dbUriOf = hUri . handleOf @"a-postgres-db" Proxy . handles
  fmap isJust $ flip DB.fetch cid $ dbUriOf sh


{-| The full test fixture.

It allows tests to

- use the servant client to invoke the backend
- check the state of service backends via the @ProcHandles@ in the 'ServerHandle'.

-}
type Fixture = (ServerHandle ('[TmpPostgres, TmpRedis]), ClientEnv)


mkFixture :: IO (ServerHandle ('[TmpPostgres, TmpRedis]), ClientEnv)
mkFixture = do
  let mkApp someHandles = do

        -- handleOf can obtain a handle using either the Proc type ...
        let redisH = handleOf @TmpRedis Proxy someHandles

            -- or the Name of it's Proc type
            dbLoc  = hUri $ handleOf @"a-postgres-db" Proxy someHandles

        -- Create the database schema
        DB.migrateDB dbLoc `onException` terminateAll someHandles

        -- Determine the redis location
        cacheLoc <- cacheLocFrom redisH  `onException` terminateAll someHandles

        pure $ waiApp dbLoc cacheLoc

  sh <- runServer testProcs mkApp
  clientEnv <- clientEnvOf sh
  pure (sh, clientEnv)


shutdown' :: (ServerHandle ('[TmpPostgres, TmpRedis]), ClientEnv) -> IO ()
shutdown' (sh, _) = shutdown sh


cacheLocFrom :: ProcHandle TmpRedis -> IO Cache.Locator
cacheLocFrom handle = case parseConnectInfo $ C8.unpack $ hUri handle of
  Left _  -> fail "Bad redis URI"
  Right x -> pure x


clientEnvOf :: AreProcs procs => ServerHandle procs -> IO ClientEnv
clientEnvOf s = do
  mgr <- newManager tlsManagerSettings
  pure $ mkClientEnv mgr $ BaseUrl Http "localhost" (serverPort s) ""


testContact :: Contact
testContact = Contact
  { contactName = "Bond"
  , contactEmail = "james@hmss.gov.uk"
  , contactAge = 45
  , contactTitle = "Mr"
  }

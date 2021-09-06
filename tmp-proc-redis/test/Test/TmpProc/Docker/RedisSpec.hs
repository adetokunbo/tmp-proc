{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeApplications  #-}
module Test.TmpProc.Docker.RedisSpec where

import           Test.Hspec

import           Control.Exception           (onException)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as C8
import           Data.Proxy                  (Proxy (..))
import qualified Data.Text                   as Text
import           Database.Redis              (exists, runRedis, setex)

import           System.TmpProc.Docker
import           System.TmpProc.Docker.Redis


-- | Used as pending alternative when docker is unavailable.
noDockerSpec :: String -> Spec
noDockerSpec desc = describe desc $
  it "cannot run as docker is unavailable" pending


spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc:Redis:" ++ Text.unpack (nameOf $ TmpRedis [])
  if noDocker then noDockerSpec desc else checkRedis desc


checkRedis :: String -> Spec
checkRedis desc =  beforeAll setupHandles $ afterAll terminateAll $ do
  describe desc $ do
    context "when using the Proc from the HList by its 'Name'" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"a-redis-db" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        context "before resetting, the test key" $ do
          it "should exist" $ \hs
            -> (checkTestKey $ named @"a-redis-db" Proxy hs) `shouldReturn` True

        it "should succeed" $ \hs
          -> ixReset @"a-redis-db" Proxy hs `shouldReturn` ()

        context "after resetting, the test key" $ do
          it "should not exist" $ \hs
            -> (checkTestKey $ named @"a-redis-db" Proxy hs) `shouldReturn` False


theProc :: HList '[TmpRedis]
theProc = TmpRedis [testKey] `HCons` HNil


setupHandles :: IO (HList '[ProcHandle TmpRedis])
setupHandles = do
  handles <- startupAll theProc
  initRedis handles `onException` terminateAll handles
  pure handles


initRedis :: HList '[ProcHandle TmpRedis] -> IO ()
initRedis = addTestKeyValue . named @"a-redis-db" Proxy


addTestKeyValue :: ProcHandle TmpRedis -> IO ()
addTestKeyValue handle = withTmpConnection handle $ \conn -> do
  liftIO (runRedis conn $ setex testKey 100 testValue) >>= \case
    Left e  -> fail $ "redis operation failed: " ++ show e
    Right _ -> pure ()


testKey :: C8.ByteString
testKey = "test.redis.key"


testValue :: C8.ByteString
testValue = "the test value"


checkTestKey :: ProcHandle TmpRedis -> IO Bool
checkTestKey handle = withTmpConnection handle $ \conn -> do
  liftIO (runRedis conn $ exists testKey) >>= \case
    Left e  -> fail $ "redis operation failed: " ++ show e
    Right x -> pure x

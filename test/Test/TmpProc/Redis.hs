{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.TmpProc.Redis where

import           Test.Hspec

import           Control.Exception           (throwIO)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as C8
import qualified Data.Text                   as Text
import           Database.Redis              (exists, runRedis, setex)
import           Network.Wai                 (Application)

import           System.Docker.TmpProc
import           System.Docker.TmpProc.Redis

import           Test.SimpleServer           (mkTestApp)
import           Test.TmpProc.Hspec          (noDockerSpec)


spec :: Bool -> Spec
spec noDocker = do
  let desc = "redis: image " ++ (Text.unpack $ procImageName testTmpProc)
  if noDocker then noDockerSpec desc else do
    beforeAll setupRds $ afterAll cleanup $ do
      describe desc $ do
        context "invoking a simple redis action" $ do
          it "should not fail" $ \oh -> do
            reset (procImageName testTmpProc) oh `shouldReturn` ()


testWaiApp :: Handle -> IO Application
testWaiApp = mkTestApp doSetup (reset $ procImageName testTmpProc)


testTmpProc :: TmpProc
testTmpProc = mkNoResetProc
  { procReset = checkTestKey
  }


setupRds :: IO Handle
setupRds = do
  h <- setupProcs [testTmpProc]
  doSetup h
  pure h


doSetup :: Handle -> IO ()
doSetup h = case procURI (procImageName testTmpProc) h of
  Left e    -> throwIO e
  Right uri -> addTestKeyValue uri


-- | Create the test table and insert some data into it
addTestKeyValue :: ProcURI -> IO ()
addTestKeyValue rdsUri = do
  withConnectionFrom rdsUri $ \conn -> do
    (liftIO $ runRedis conn $ setex testKey 100 testValue) >>= \case
      Left e -> fail $ "redis operation failed: " ++ show e
      Right _ -> pure ()


checkTestKey :: ProcURI -> IO ()
checkTestKey rdsUri = do
  withConnectionFrom rdsUri $ \conn -> do
    (liftIO $ runRedis conn $ exists testKey) >>= \case
      Left e -> fail $ "redis operation failed: " ++ show e
      Right True -> pure ()
      Right _ -> fail "Could not find the test key"


testKey :: C8.ByteString
testKey = "test.redis.key"

testValue :: C8.ByteString
testValue = "the test value"

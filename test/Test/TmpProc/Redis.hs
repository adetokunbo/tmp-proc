{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.TmpProc.Redis where

import           Test.Hspec

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (throwIO)
import           Control.Monad               (forever)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as C8
import qualified Data.Text                   as Text

import           Database.Redis              (runRedis, setex)
import           System.Docker.TmpProc
import           System.Docker.TmpProc.Redis

import           Test.NoopServer             (noopPort)
import           Test.TmpProc.Hspec          (noDockerSpec)


spec :: Bool -> Spec
spec noDocker = do
  let desc = "redis: image " ++ (Text.unpack $ procImageName resetProc)
  if noDocker then noDockerSpec desc else do
    beforeAll (rdsSetup resetProc) $ afterAll cleanup $ do
      describe desc $ do
        context "invoking a simple redis action" $ do
          it "should not fail" $ \oh -> do
            reset (procImageName resetProc) oh `shouldReturn` ()


-- | A testProc that executes the user-provided reset action.
resetProc :: TmpProc
resetProc = mkNoResetProc
  { procReset = resetAction
  }


rdsServer :: OwnerHandle -> Port -> IO () -> IO ()
rdsServer oh _port ready = do
  let loopPeriod = 5000000
      tryUri = procURI (procImageName resetProc) oh
  case tryUri of
    Left e    -> throwIO e
    Right uri -> addTestKeyValue uri
  ready
  forever $ threadDelay loopPeriod


-- | Create the test table and insert some data into it
addTestKeyValue :: ProcURI -> IO ()
addTestKeyValue rdsUri = do
  withConnectionFrom rdsUri $ \conn -> do
    _ <- liftIO $ runRedis conn $ setex testKey 100 testValue
    pure ()


resetAction :: ProcURI -> IO ()
resetAction = clearKeys [testKey]


testKey :: C8.ByteString
testKey = "test.redis.key"

testValue :: C8.ByteString
testValue = "the test value"


rdsOwner :: Owner IOError
rdsOwner = Owner rdsServer $ const $ pure $ Right ()


rdsSetup :: TmpProc -> IO OwnerHandle
rdsSetup tp = setup rdsOwner [tp] noopPort

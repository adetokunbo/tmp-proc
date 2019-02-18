{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.TmpProc.Postgres where

import           Test.Hspec

import           Control.Concurrent             (threadDelay)
import           Control.Exception              (throwIO)
import           Control.Monad                  (forever, void)
import qualified Data.Text                      as Text

import           Database.PostgreSQL.Simple     (connectPostgreSQL, execute_)
import           System.Docker.TmpProc
import           System.Docker.TmpProc.Postgres

import           Test.NoopServer                (noopPort)


spec :: Spec
spec = do
  let desc = "postgres: image " ++ (Text.unpack $ procImageName resetProc)
  beforeAll (pgSetup resetProc) $ afterAll cleanup $ do
    describe desc $ do
      context "invoking a simple SQL reset action" $ do
        it "should not fail" $ \oh -> do
          reset (procImageName resetProc) oh `shouldReturn` ()


-- | A testProc that executes the user-provided reset action.
resetProc :: TmpProc
resetProc = mkNoResetProc
  { procReset = resetAction
  }


pgServer :: OwnerHandle -> Port -> IO () -> IO ()
pgServer oh _port ready = do
  let loopPeriod = 5000000
      tryUri = procURI (procImageName resetProc) oh
  case tryUri of
    Left e    -> throwIO e
    Right uri -> createTestTable uri
  ready
  forever $ threadDelay loopPeriod


-- | Create the test table and insert some data into it
createTestTable :: ProcURI -> IO ()
createTestTable pgUri = do
  c <- connectPostgreSQL pgUri
  let commands =
        [ "CREATE TABLE test_tmp_proc (an_id INTEGER)"
        , "INSERT INTO test_tmp_proc VALUES (347)"
        ]
  mapM_ (execute_ c) commands


resetAction :: ProcURI -> IO ()
resetAction pgUri = do
  c <- connectPostgreSQL pgUri
  void $ execute_ c "DELETE FROM test_tmp_proc"


pgOwner :: Owner IOError
pgOwner = Owner pgServer $ const $ pure $ Right ()


pgSetup :: TmpProc -> IO OwnerHandle
pgSetup tp = setup pgOwner [tp] noopPort

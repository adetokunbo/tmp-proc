{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.TmpProc.Postgres where

import           Test.Hspec

import           Control.Exception              (throwIO)
import           Control.Monad                  (void)
import qualified Data.Text                      as Text
import           Database.PostgreSQL.Simple     (connectPostgreSQL, execute_)
import           Network.Wai                    (Application)

import           System.Docker.TmpProc
import           System.Docker.TmpProc.Postgres

import           Test.SimpleServer              (mkTestApp)
import           Test.TmpProc.Hspec             (noDockerSpec)


spec :: Bool -> Spec
spec noDocker = do
  let desc = "postgres: image " ++ (Text.unpack $ procImageName testTmpProc)
  if noDocker then noDockerSpec desc else do
    beforeAll setupDb $ afterAll cleanup $ do
      describe desc $ do
        context "invoking a simple SQL reset action" $ do
          it "should not fail" $ \oh -> do
            reset (procImageName testTmpProc) oh `shouldReturn` ()


testWaiApp :: Handle -> IO Application
testWaiApp = mkTestApp doSetup (reset $ procImageName testTmpProc)


testTmpProc :: TmpProc
testTmpProc = mkNoResetProc
  { procReset = emptyTheTestTable
  }


setupDb :: IO Handle
setupDb = do
  h <- setupProcs [testTmpProc]
  doSetup h
  pure h


doSetup :: Handle -> IO ()
doSetup h = case procURI (procImageName testTmpProc) h of
  Left e    -> throwIO e
  Right uri -> createTestTable uri


-- | Create the test table and insert some data into it
createTestTable :: ProcURI -> IO ()
createTestTable pgUri = do
  c <- connectPostgreSQL pgUri
  let commands =
        [ "CREATE TABLE test_tmp_proc (an_id INTEGER)"
        , "INSERT INTO test_tmp_proc VALUES (347)"
        ]
  mapM_ (execute_ c) commands


emptyTheTestTable :: ProcURI -> IO ()
emptyTheTestTable pgUri = do
  c <- connectPostgreSQL pgUri
  void $ execute_ c "DELETE FROM test_tmp_proc"

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Test.TmpProc.Docker.PostgresSpec where

import           Test.Hspec
import           Test.Hspec.TmpProc

import           Control.Exception              (onException)
import           Data.Proxy                     (Proxy (..))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           Database.PostgreSQL.Simple     (execute_)

import           System.TmpProc.Docker
import           System.TmpProc.Docker.Postgres


spec :: Spec
spec = tdescribe desc $ do
  beforeAll setupHandles $ afterAll terminateAll $ do
    context "when using the Proc from the HList by its 'Name'" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"a-postgres-db" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        it "should succeed" $ \hs
          -> ixReset @"a-postgres-db" Proxy hs `shouldReturn`()


setupHandles :: IO (HList '[ProcHandle TmpPostgres])
setupHandles = do
  handles <- startupAll $ testProc `HCons` HNil
  initTable handles `onException` terminateAll handles
  pure handles


testProc :: TmpPostgres
testProc = TmpPostgres [testTable]


testTable :: Text
testTable = "to_be_reset"


initTable :: HList '[ProcHandle TmpPostgres] -> IO ()
initTable = createTestTable . connected @"a-postgres-db" Proxy


createTestTable :: ProcHandle TmpPostgres -> IO ()
createTestTable handle = withTmpConn handle $ \c -> do
  let commands =
        [ "CREATE TABLE to_be_reset (an_id INTEGER)"
        , "INSERT INTO to_be_reset VALUES (347)"
        ]
  mapM_ (execute_ c) commands


desc :: String
desc = "Tmp.Proc:Postgres:" ++ Text.unpack (nameOf testProc)

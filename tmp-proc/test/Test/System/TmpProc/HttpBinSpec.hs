{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.System.TmpProc.HttpBinSpec where

import           Test.Hspec

import           Control.Exception         (catch)
import qualified Data.ByteString.Char8     as C8
import           Data.List                 (foldl')
import           Data.Proxy                (Proxy (Proxy))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Network.HTTP.Req

import           System.TmpProc.Docker     (HList (..), HostIpAddress,
                                            Proc (..), ProcHandle (..), SvcURI,
                                            ixPing, ixReset, nameOf, startupAll,
                                            terminateAll)
import           Test.System.TmpProc.Hspec (noDockerSpec)


setupHandles :: IO (HList '[ProcHandle HttpBinTest])
setupHandles = startupAll $ HttpBinTest `HCons` HNil


spec :: Bool -> Spec
spec noDocker = do
  let desc = "Tmp.Proc: " ++ Text.unpack (nameOf HttpBinTest)
  if noDocker then noDockerSpec desc else checkHttpBin desc


checkHttpBin :: String -> Spec
checkHttpBin desc =  beforeAll setupHandles $ afterAll terminateAll $ do
  describe desc $ do
    context "when using the Proc from the HList by Name" $ do

      context "ixPing" $ do

        it "should succeed" $ \hs
          -> ixPing @"http-bin-test" Proxy hs `shouldReturn`()

      context "ixReset" $ do

        it "should succeed" $ \hs
          -> ixReset @"http-bin-test" Proxy hs `shouldReturn`()


{-| A data type representing a connection to the HttpBin server. -}
data HttpBinTest = HttpBinTest

{-| Run HttpBin as temporary process.  -}
instance Proc HttpBinTest where
  type Image HttpBinTest = "kennethreitz/httpbin"
  type Name HttpBinTest = "http-bin-test"

  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


{-| Make a uri access the http-bin server. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


{-| Does nothing, httpBin is stateless. -}
ping' :: ProcHandle HttpBinTest -> IO ()
ping' handle = do
  let catchHttp x = x `catch` (\(_ :: HttpException) ->
                                  fail "tmp proc:httpbin:ping failed")
  catchHttp $ do
    gotStatus <- statusOfGet handle "/status/200"
    if (gotStatus == 200) then pure () else
      fail "tmp proc:httpbin:incorrect ping status"


-- | Determine the status from a Get on localhost.
statusOfGet :: ProcHandle HttpBinTest -> Text -> IO Int
statusOfGet handle urlPath = runReq defaultHttpConfig $ do
  r <- req GET (handleUrl handle urlPath) NoReqBody ignoreResponse $ mempty
  return $ responseStatusCode r


handleUrl :: ProcHandle HttpBinTest -> Text -> Url 'Http
handleUrl handle urlPath = foldl' (/:) (http $ hAddr handle)
  $ Text.splitOn "/" $ Text.dropWhile (== '/') urlPath

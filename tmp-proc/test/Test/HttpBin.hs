{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.HttpBin where

import           Control.Exception     (catch)
import qualified Data.ByteString.Char8 as C8
import           Data.List             (foldl')
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Network.HTTP.Req

import           System.TmpProc.Docker (HList (..), HostIpAddress, Proc (..),
                                        ProcHandle (..), SvcURI, startupAll)


setupHandles :: IO (HList '[ProcHandle HttpBinTest])
setupHandles = startupAll $ HttpBinTest `HCons` HNil


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
    gotStatus <- handleGet handle "/status/200"
    if (gotStatus == 200) then pure () else
      fail "tmp proc:httpbin:incorrect ping status"


-- | Determine the status from a Get on localhost.
handleGet :: ProcHandle HttpBinTest -> Text -> IO Int
handleGet handle urlPath = runReq defaultHttpConfig $ do
  r <- req GET (handleUrl handle urlPath) NoReqBody ignoreResponse $ mempty
  return $ responseStatusCode r


handleUrl :: ProcHandle HttpBinTest -> Text -> Url 'Http
handleUrl handle urlPath = foldl' (/:) (http $ hAddr handle)
  $ Text.splitOn "/" $ Text.dropWhile (== '/') urlPath

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Test.HttpBin where

import           Control.Exception        (catch)
import qualified Data.ByteString.Char8    as C8
import           Data.List                (foldl')
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Network.HTTP.Req

import           System.TmpProc.Docker    (HList (..), HostIpAddress, Proc (..),
                                           Proc2Handle, ProcHandle (..), SvcURI,
                                           startupAll)
import           System.TmpProc.TypeLevel ((%:))


setupHandles :: IO (HList (Proc2Handle '[HttpBinTest, HttpBinTest2, HttpBinTest3]))
setupHandles = startupAll $ HttpBinTest %: HttpBinTest2 %: HttpBinTest3 %: HNil


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


{-| Another data type representing a connection to the HttpBin server.


Allows the test tmp process list used to have multiple entries, to better
detect any compiler errors in the tmp-proc library.

-}
data HttpBinTest2 = HttpBinTest2

{-| Run HttpBin as temporary process.  -}
instance Proc HttpBinTest2 where
  type Image HttpBinTest2 = "kennethreitz/httpbin"
  type Name HttpBinTest2 = "http-bin-test-2"

  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'


{-| Yet another data type representing a connection to the HttpBin server.


Allows the test tmp process list used to have multiple entries, to better
detect any compiler errors in the tmp-proc library.

-}
data HttpBinTest3 = HttpBinTest3

{-| Run HttpBin as temporary process.  -}
instance Proc HttpBinTest3 where
  type Image HttpBinTest3 = "kennethreitz/httpbin"
  type Name HttpBinTest3 = "http-bin-test-3"

  uriOf = mkUri'
  runArgs = []
  reset _ = pure ()
  ping = ping'



{-| Make a uri access the http-bin server. -}
mkUri' :: HostIpAddress -> SvcURI
mkUri' ip = "http://" <> C8.pack (Text.unpack ip) <> "/"


{-| Does nothing, httpBin is stateless. -}
ping' :: ProcHandle a -> IO ()
ping' handle = do
  let catchHttp x = x `catch` (\(_ :: HttpException) ->
                                  fail "tmp proc:httpbin:ping failed")
  catchHttp $ do
    gotStatus <- handleGet handle "/status/200"
    if (gotStatus == 200) then pure () else
      fail "tmp proc:httpbin:incorrect ping status"


-- | Determine the status from a Get on localhost.
handleGet :: ProcHandle a -> Text -> IO Int
handleGet handle urlPath = runReq defaultHttpConfig $ do
  r <- req GET (handleUrl handle urlPath) NoReqBody ignoreResponse $ mempty
  return $ responseStatusCode r


handleUrl :: ProcHandle a -> Text -> Url 'Http
handleUrl handle urlPath = foldl' (/:) (http $ hAddr handle)
  $ Text.splitOn "/" $ Text.dropWhile (== '/') urlPath

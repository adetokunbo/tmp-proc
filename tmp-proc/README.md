# tmp-proc

[![GitHub CI](https://github.com/adetokunbo/tmp-proc/actions/workflows/test.yml/badge.svg)](https://github.com/adetokunbo/tmp-proc/actions)
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc/LICENSE)

`tmp-proc` is a small library designed to simplify integration tests that use
services running on docker.

This README contains a _How to_ tutorial on using this library. This tutorial
explains step by step how to specify a docker image as `tmp proc` and use it in
a test.  __N.B.__ It assumes that docker is installed.

All code below can be compiled and run with the following commands:

```shell
$ cabal new-build tmp-proc
$ cabal new-exec readme
```

## Preamble: imports and language extensions

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

import           Test.Hspec

import qualified Data.ByteString.Char8 as C8
import           Data.List             (foldl')
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Network.HTTP.Req

import           System.TmpProc        (HList (..), HandlesOf, HostIpAddress,
                                        Pinged (..), Proc (..), ProcHandle (..),
                                        SvcURI, manyNamed, startupAll, toPinged,
                                        (&:), ixPing, nameOf, terminateAll)

```


## Specify a Proc instance

In 'tmp-proc', docker instances are specified by making new instances of the
'Proc' typeclass.

For this tutorial, we'll test the famous [http-bin](https://httpbin.org)
service. Although it's an online service in it's own right, it is also available
as a docker image.


```haskell
data HttpBinLhs = HttpBinLhs

instance Proc HttpBinLhs where
  type Image HttpBinLhs = "kennethreitz/httpbin"
  type Name HttpBinLhs = "http-bin-lhs"

  uriOf ip = "http://" <> C8.pack (Text.unpack ip) <> "/"
  runArgs = []
  reset _ = pure ()
  ping = pingImpl

```

A `Proc` instance specifies both an `Image` and `Name`.
- The `Image` corresponds to the docker image that needs to be run
- The `Name` is a label that needs to be unique; it is used to index the Proc instance.

The instance also specifies a number of typeclass functions; only `ping` will be
mentioned in this tutorial, but the others all have important roles.


```haskell

pingImpl :: ProcHandle a -> IO Pinged
pingImpl handle = toPinged @HttpException Proxy $ do
  gotStatus <- runReq defaultHttpConfig $ do
    r <- req GET (handleUrl handle "/status/200") NoReqBody ignoreResponse $ mempty
    pure $ responseStatusCode r
  if (gotStatus == 200) then pure OK else pure NotOK


handleUrl :: ProcHandle a -> Text -> Url 'Http
handleUrl handle urlPath = foldl' (/:) (http $ hAddr handle)
  $ Text.splitOn "/" $ Text.dropWhile (== '/') urlPath


```

The `pingImpl` used above by `Proc` instance is implemented next. Each `Proc`
instance must provide valid `ping` implementation,

`tmp-proc` uses these to determine when the `Proc's` service is ready for use in
the test.

## Use the Proc in an hspec Spec


```haskell

spec :: Spec
spec = describe ("Tmp.Proc: " ++ Text.unpack (nameOf HttpBinLhs)) $ do
  beforeAll (startupAll $ HttpBinLhs &: HNil) $ afterAll terminateAll $ do
    context "When accessing the services in the list of test tmp procs" $ do

      context "ixPing" $ do

        it "should succeed when accessing a Proc by name" $ \handles
          -> ixPing @"http-bin-lhs" Proxy handles `shouldReturn` OK

        it "should succeed when accessing a Proc by type" $ \handles
          -> ixPing @HttpBinLhs Proxy handles `shouldReturn` OK

```

With just this, it's now possible to write a simple test that shows how

- hspec launches Procs during test setup, making part of the test fixture

   - `startupAll` performs the startup; it takes a HList (heterogenous list) of
     different 'Proc' types and starts them all up, ensuring they all start-up ok
     or failing the test.

   - once setup has succeeded, each test is passed the handles created by `startupAll`

   - the test can use the handles to access the running 'Proc'.

   - `tmp-proc` provides useful combinators that simplify working with the handles.



## Run the Spec

```haskell

main :: IO ()
main = hspec spec

```


[1]: https://hackage.haskell.org/package/wai
[2]: https://hspec.github.io
[3]: https://hackage.haskell.org/package/tasty
[4]: https://github.com/adetokunbo/tmp-proc/tree/master/tmp-proc-example
[5]: https://github.com/adetokunbo/tmp-proc/tree/master/tmp-proc

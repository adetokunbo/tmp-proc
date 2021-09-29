# tmp-proc

[![Hackage](https://img.shields.io/hackage/v/tmp-proc.svg)](https://hackage.haskell.org/package/tmp-proc)
[![GitHub CI](https://github.com/adetokunbo/tmp-proc/actions/workflows/test.yml/badge.svg)](https://github.com/adetokunbo/tmp-proc/actions)
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc/LICENSE)

`tmp-proc` is a small library designed to simplify integration tests that use
services running on docker.

This README contains a _How to_ tutorial on using this library. This tutorial
explains step by step how to specify a docker image as a `tmp proc` and use it in
a test.

__N.B.__ It assumes that docker is installed.

All code below can be compiled and run with the following commands:

```shell
$ cabal new-build tmp-proc
$ cabal new-exec readme # at the moment (29/09/2021), this is disabled due to a bug
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

In `tmp-proc`, docker instances are specified by making new instances of the
`Proc` typeclass.

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
- The `Name` is a label that needs to be unique; it is used an alternate index for the Proc instance.

The instance also specifies a number of useful typeclass functions; only `ping`
will be covered in this tutorial, but the others all have important roles.


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

The `pingImpl` used by the `Proc` instance above is implemented next.

Each `Proc` instance must provide a valid `ping` implementation, `tmp-proc` uses
`ping` to determine when the `Proc's` service is ready for use in the test.

## Using the Proc


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

With just this, it's now possible to write a simple test showing various
features of `tmp-proc`.  E.g,

- `hspec` launches `Procs` during test setup

   - this results in an `HList` of `ProcHandle` types being passed to each test.

     - (In this example, the HList has only one `Proc`. `startupAll` allows for
       many `Procs` to started, each of different type. This is possible because
       `startupAll` acts on and returns a heteregenous list (or `HList`) rather
       than the usual `List` type.

   - `startupAll` takes an `HList` of `Procs` and returns an `HList` of
     corresponding `ProcHandle` types, first ensuring that all the corresponding
     docker services start up ok

- once setup succeeds, each test is passed the `ProcHandles` created by
  `startupAll`

   - this is powered by a feature of [hspec][2], it's through use of `hspec's`
     `beforeAll` hook that enables this

    - `startupAll` is just one example of a `tmp-proc` combinator meshing well with
    the test frameworks' combinators.

        - the `startAll` and `terminateAll` functions used here also work with
          `tasty's` [withResource][6]

        - `tmp-proc` provides other functions that work with other hooks in [hspec][1]

- the test cases here show the way that `tmp-proc` functions work with an
  `HList` of `ProcHandles`

    - `ixPing` uses [TypeApplications][7] with an index type to identify the
        'ProcHandle' to ping

    - `tmp-proc` provides similar functions that enable access to attributes
        of one or many `ProcHandle`.

    - Though not shown here, as well as accessing a `ProcHandle's` attributes,
      test code may also to access the corresponding service using a
      `Connection` type specific to that service.

    - __N.B.__ these test cases are completely unrealistic; there should be no
      reason to use `ixPing` in a normal test case! For slightly more realism,
      please take a look at the [example package][4].


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
[6]: https://hackage.haskell.org/package/tasty-1.4.2/docs/Test-Tasty.html#v:withResource
[7]: https://typeclasses.com/ghc/type-applications

# tmp-proc

[![GitHub CI](https://github.com/adetokunbo/tmp-proc/actions/workflows/test.yml/badge.svg)](https://github.com/adetokunbo/tmp-proc/actions)
[![Stackage Nightly][stackage-nightly-badge]][stackage-nightly]
[![Hackage][hackage-badge]][hackage]
[![Hackage Dependencies][hackage-deps-badge]][hackage-deps]
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc/LICENSE)

`tmp-proc` is a small library designed to simplify integration tests that use
services running on docker.

This README contains a _How To_ tutorial on using this library. This tutorial
explains step by step how to specify a docker image as a `tmp proc` and use it in
a test.

__N.B.__ It assumes that docker is installed.

All code below can be compiled and run with the following commands:

```shell
# At the moment (30/09/2021), this is does not consistently run when using cabal
$ stack build tmp-proc
$ stack run readme
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
[Proc][17] typeclass.

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

An instance specifies a number of useful typeclass functions; only [ping][18] is
covered in this tutorial, however they all have important roles that support
integration testing.


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

  ```ignore
  ...
  beforeAll (startupAll $ HttpBinLhs &: HNil) $ afterAll terminateAll $ do
  ```

   - this results in an `HList` of [ProcHandle][9] types being passed to each test.


- once setup succeeds, each test is passed the `ProcHandles` created by
  `startupAll`

    ```ignore
    ...
    it "should succeed when accessing a Proc by name" $ \handles
    ```

   - this is simply using the behavior of the `beforeAll` [hook][8] of
    [hspec][2], and is one example of how `tmp-proc` combinators mesh well with
    typical test frameworks' combinators.

    - the `startupAll` and `terminateAll` functions used here also work with
      `tasty's` [withResource][6]

    - `tmp-proc` provides other functions that work with other [hspec hooks][8],
      e.g, [runServer][15] and others in [System.TmpProc.Warp][16] that simplify
      testing with [WAI][1].

    - __N.B.__ In this an example, the HList has only one `Proc`.
      [startupAll][13] allows for many `Procs` to be started, each of a
      different type. This is possible because `startupAll` acts on and returns
      a heteregenous list (`HList`) rather than the usual `List` type. Before it
      completes, `startupAll` ensures all the docker services start up ok.

- the test cases here show the way that the `tmp-proc` functions use an
  `HList` of `ProcHandles` to interact with the launched services

    ```ignore
    ...
    ixPing @"http-bin-lhs" Proxy handles `shouldReturn` OK
    ```

    - `ixPing` uses [TypeApplications][7] with an index type to identify the
        [ProcHandle][9] to ping

    - `tmp-proc` provides similar functions that enable access to attributes
        of one or many `ProcHandle` in an `HList`.

    - Though not shown here, as well as accessing a `ProcHandle's` attributes,
      test code may also access the corresponding service using a
      [Connection][10] type specific to that service.


- __N.B.__ these test cases are completely unrealistic; there should be no
  reason to use `ixPing` in a normal test case! For slightly more realism,
  please take a look at the examples that use [hspec][11] or [tasty][12] in the
  [example package][4].


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
[8]: https://hspec.github.io/writing-specs.html#using-hooks
[9]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#t:ProcHandle
[10]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#v:withTmpConn
[11]: https://hackage.haskell.org/package/tmp-proc-example-0.5.0.0/docs/src/TmpProc.Example2.IntegrationSpec.html#spec
[12]: https://hackage.haskell.org/package/tmp-proc-example-0.5.0.0/docs/src/TmpProc.Example1.IntegrationTaste.html#tests
[13]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#v:startupAll
[14]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#v:startup
[15]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Warp.html#v:runServer
[16]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Warp.html
[17]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#t:Proc
[18]: https://hackage.haskell.org/package/tmp-proc-0.5.0.1/docs/System-TmpProc-Docker.html#v:ping

[hackage-deps-badge]:     <https://img.shields.io/hackage-deps/v/tmp-proc.svg>
[hackage-deps]:           <http://packdeps.haskellers.com/feed?needle=tmp-proc>
[hackage-badge]:          <https://img.shields.io/hackage/v/tmp-proc.svg>
[hackage]:                <https://hackage.haskell.org/package/tmp-proc>
[stackage-nightly-badge]: <http://stackage.org/package/tmp-proc/badge/nightly>
[stackage-nightly]:       <http://stackage.org/nightly/package/tmp-proc>

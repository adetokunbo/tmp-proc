# tmp-proc

[![GitHub CI](https://github.com/adetokunbo/tmp-proc/actions/workflows/cabal.yml/badge.svg)](https://github.com/adetokunbo/tmp-proc/actions)
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc/LICENSE)

`tmp-proc` is a testing library designed to simplify integration tests that use
services running on docker.

## Goals

While it is already possible to write integration tests that use dockerized
services using the various haskell testing frameworks, `tmp-proc` aims to make
doing so easier by providing useful types and combinators that take care of
various things that contribute to the boilerplate involved while coding
integration tests.

It provides useful combinators that simplify

  * launching multiple services on docker during test setup
  * connecting to those services from a [WAI][1] server in the integration test
  * getting connections to the launched services to query their state

`tmp-proc's` combinators allow for easy integration with many of Haskell's
popular testing frameworks.

## Quick Start

To get started, please read through this [quick HOWTO][5].

After that, you can learn more through a detailed example available as a
separate [package][4]. It's a working example of testing a simple
postgresql/redis servant-based API service, with examples of usage from both
[hspec][2] and [tasty][3].


## Similar libraries

`tmp-proc` provides a similar set of features as [testcontainers][6] library. If
`tmp-proc` interests you, you might want to check that out too.


[1]: https://hackage.haskell.org/package/wai
[2]: https://hackage.haskell.org/package/tmp-proc-example-0.5.0.0/docs/src/TmpProc.Example2.IntegrationSpec.html#spec
[3]: https://hackage.haskell.org/package/tmp-proc-example-0.5.0.0/docs/src/TmpProc.Example1.IntegrationTaste.html#tests
[4]: https://github.com/adetokunbo/tmp-proc/tree/master/tmp-proc-example
[5]: https://github.com/adetokunbo/tmp-proc/tree/master/tmp-proc#readme
[6]: https://hackage.haskell.org/package/testcontainers

# Revision history for tmp-proc

`tmp-proc` uses [PVP Versioning][1].

## 0.6.99.0 -- 2024-04-07

* Unreleased

## 0.6.1.0 -- 2024-03-14

* Extend the version bounds of tls to allow 2.1
* Drop support for compiling with GHC 8.8.4

## 0.6.0.1 -- 2024-02-28

* Extend the version bounds of bytestring to allow 0.12.1

## 0.6.0.0 -- 2024-01-09

* Removed SomeProcs

## 0.5.3.0 -- 2023-08-11

* Add HList constructors `only` and `both` (alias: `&:&`)

## 0.5.2.1 -- 2023-07-17

* Avoid non-building dependencies in the testable README

## 0.5.2.0 -- 2023-07-12

* Bump minimum required version of warp-tls
* Refactor/Disable tests to avoid direct/indirect dependencies on
  Network.Connection

## 0.5.1.4 -- 2023-07-12

* Extend the version bounds of bytestring to allow 0.12

## 0.5.1.3 -- 2022-12-06

* Extend the version bounds of mtl to allow 2.3.1

## 0.5.1.2 -- 2022-08-11

*  Relax version bounds

## 0.5.1.0 -- 2022-08-08

* Bump version of text and base

## 0.5.0.1 -- 2021-09-30

* Fix use of packaged data in tests
* Introduce some build flags to control how the package builds in CI environments


## 0.5.0.0 -- 2021-09-28

* Initial release to hackage

* Re-implemented the user surface to be more typeful and hopefully easier to use.

* Switched the development build environment to haskell.nix

## 0.4.0.0 -- 2021-08-03

* Update versions of major dependencies, allowing it to build with GHC 8.10

## 0.3.2.0 -- 2019-04-01

* Make the run*Server functions throw exceptions in app threads to the calling
  thread.

## 0.3.1.0 -- 2019-02-26

* Add new public functions that allow TLS-protected endpoints

## 0.3.0.0 -- 2019-02-25

* Reorganize the public API for simpler usage with HSpec and Tasty


## 0.2.0.0 -- 2019-02-18

* Added integration tests, removed unnecessary internal features from the public
  api.

## 0.1.0.0 -- 2019-02-17

* First version. Extracted from some a non-public test library

[1]: https://pvp.haskell.org

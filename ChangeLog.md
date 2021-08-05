# Revision history for docker-tmp-proc

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

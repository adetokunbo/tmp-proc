# tmp-proc-postgres

[![Hackage](https://img.shields.io/hackage/v/tmp-proc-postgres.svg)](https://hackage.haskell.org/package/tmp-proc-postgres)
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc-postgres/LICENSE)

`tmp-proc-postgres` provides an example of using `tmp-proc` to launch dockerized
PostgreSQL in integration tests.

It provides an instance of [Proc][1] for launching the PostgreSQL database docker image,
but also allows

  * configuration of simple reset behaviour to be enabled in tests
  * an instance of [Conn][2] that simplifies opening connections to the database from
    tests that use `tmp-proc`

[1]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Proc
[2]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Conn

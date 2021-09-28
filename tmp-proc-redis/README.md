# tmp-proc-redis

`tmp-proc-redis` provides an example of using `tmp-proc` to launch dockerized
Redis in integration tests.

It provides an instance of [Proc][1] for launching the Redis database docker image,
but also allows

  * configuration of simple reset behaviour to be enabled in tests
  * an instance of [Conn][2] that simplifies opening connections to the database from
    tests that use `tmp-proc`

[1]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Proc
[2]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Conn

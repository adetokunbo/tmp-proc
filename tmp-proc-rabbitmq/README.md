# tmp-proc-rabbitmq

`tmp-proc-rabbitmq` provides an example of using `tmp-proc` to launch dockerized
RabbitMQ in integration tests.

It provides an instance of [Proc][1] for launching the RabbitMQ image, and also
provides an instance of [Conn][2] that simplifies opening connections to the
launched RabbitMQ service from tests that use `tmp-proc`.

[1]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Proc
[2]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Conn

# tmp-proc-rabbitmq

[![Hackage](https://img.shields.io/hackage/v/tmp-proc-rabbitmq.svg)](https://hackage.haskell.org/package/tmp-proc-rabbitmq)
[![BSD-3.0 license](https://img.shields.io/badge/license-BSD--3.0-blue.svg)](https://github.com/adetokunbo/tmp-proc/blob/master/tmp-proc-rabbitmq/LICENSE)

`tmp-proc-rabbitmq` provides an example of using `tmp-proc` to launch dockerized
RabbitMQ in integration tests.

It provides an instance of [Proc][1] for launching the RabbitMQ image, and also
provides an instance of [Conn][2] that simplifies opening connections to the
launched RabbitMQ service from tests that use `tmp-proc`.

[1]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Proc
[2]: https://hackage.haskell.org/package/tmp-proc-0.5.0.0/docs/System-TmpProc-Docker.html#t:Conn

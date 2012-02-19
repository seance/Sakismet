Scala library for Akismet (http://akismet.com/).
------------------------------------------------

Provides the full Akismet API (http://akismet.com/development/api/) in pure functional Scala.

Basic usage:

```scala
import sakismet._

val s = Sakismet("key", "blog")
	.user_ip("ip")
	.referrer("ref")
	
s.verify_key
```

Non-blocking usage for NIO:

```scala
import sakismet._
import dispatch._

val s = Sakismet("key", "blog")(new nio.Http)
	.user_ip("ip")
	.referrer("ref")

val f = s.verify_key // f is a Dispatch non-blocking future
f()
```

Sakismet is fully type-safe and functional, containing no mutable state. It uses a type-safe builder pattern, leveraging Scala's structural types and generalized type constraints.

The Sakismet runtime is built on **Dispatch** (http://dispatch.databinder.net/), a pure Scala library for conducting HTTP interaction. Sakismet fully supports Dispatch's HttpExecutor API, allowing the user to freely switch between the Dispatch blocking and non-blocking implementations.

Sakismet is built with SBT 0.11.2 (https://github.com/harrah/xsbt) for Scala 2.9.1.

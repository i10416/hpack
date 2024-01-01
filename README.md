# http2.hpack


## About

Scala clone of Crystal lang HTTP2::HPACK module.

This is not production-ready.

```scala
import http2.hpack.*
val encoder = Encoder()
val data = encoder.encode(":method",Seq("GET"),Encoder.Opt.INDEXED)
// => Array(-126):  0x82 in signed decimal representation
val decoder = Decoder()
val result = decoder.decode(data)
println(result.toList) // => List((":method", "GET"))
```

## For Developers

```sh
git clone git@github.com:i10416/hpack.git
```

### Formatting

```sh
scala-cli fmt
```

### Run all tests

```sh
scala-cli test .
```

### Run Some Tests

```sh
scala-cli test . --test-only 'http2.hpack.*' -- '<pattern>'
```

### Generate a docsite

```sh
scala-cli  doc --default-scaladoc-opts -f --scalac-option "-private" \
  --scalac-option "-doc-root-content:`pwd`/README.md" \
  --scalac-option "-project:hpack" .
```

To see the docsite in local browser, run the following command.

```sh
cd scala-doc && python3 -m http.server
```


NOTE: scaladoc options: https://docs.scala-lang.org/scala3/guides/scaladoc/settings.html#overview-of-all-available-settings

## Refs

https://github.com/ysbaddaden/http2/tree/master/src/hpack

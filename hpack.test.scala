//> using test.dep "org.scalameta::munit::1.0.0-M7"
package http2.hpack

import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer
import http2.hpack.syntax._
import scala.util.chaining._
class T extends munit.FunSuite:
  test(
    "putSpecialHeadersFirst stably sorts headers such that headers with `:` prefix come first"
  ) {
    val headers = List(
      (":method", "GET"),
      ("custom-key", "custom-value"),
      (":authority", "www.example.com"),
      ("password", "secret"),
      (":path", "/sample/path")
    )
    val enc = Encoder()

    assertEquals(
      headers sortWith enc.putSpecialHeadersFirst,
      List(
        (":method", "GET"),
        (":authority", "www.example.com"),
        (":path", "/sample/path"),
        ("custom-key", "custom-value"),
        ("password", "secret")
      )
    )

  }
  test("decoding huffman-encoded value returns the original") {
    val h = Huffman.INSTANCE
    val original = "abcdefghijklmnopqrstuvwxyz"
    val roundtrip = h.decode(h.encode(original))
    assertEquals(roundtrip, original)
  }
  test("encode 10 to 5 bits prefix octet") {
    // C.1.1. Example 1: Encoding 10 Using a 5-Bit Prefix
    val octet = Integer.parseInt("10100000", 2).toByte
    val enc = Encoder(null.asInstanceOf)
    val buf = ArrayBuffer[Byte]()
    enc.encodeInt(10, 5, buf, octet)
    assertEquals(buf, ArrayBuffer(Integer.parseInt("10101010", 2).toByte))
  }
  test("decode 10 from 5 bits prefix octet") {
    // C.1.1. Example 1: Encoding 10 Using a 5-Bit Prefix
    val octet = Integer.parseInt("10101010", 2).toByte
    val dec = Decoder()
    val result = dec.decodeInt(Seq.empty.iterator, octet, 5)
    assertEquals(result, 10)
  }

  test("decode 42 from 8 bits prefix octet") {
    // C.1.3. Example 3: Encoding 42 Starting at an Octet Boundary
    val octet = Integer.parseInt("00101010", 2).toByte
    val dec = Decoder()
    val result = dec.decodeInt(Seq.empty.iterator, octet, 8)
    assertEquals(result, 42)
  }
  test("encode 1337 to 5 bits prefix octets") {
    // C.1.2.  Example 2: Encoding 1337 Using a 5-Bit Prefix
    val octet = Integer.parseInt("10100000", 2).toByte
    val enc = Encoder(null.asInstanceOf)
    val buf = ArrayBuffer[Byte]()
    enc.encodeInt(1337, 5, buf, octet)
    assertEquals(
      buf.map(_ & 0xff).map(_.toBinaryString).toList,
      List(
        "10111111",
        "10011010",
        /*0000*/ "1010"
      )
    )
  }
  test("decode 1337 from 5 bits prefix octets") {
    // C.1.2. Example 2: Encoding 1337 Using a 5-Bit Prefix
    val prefix = Integer.parseInt("00011111", 2).toByte
    val continue = Integer.parseInt("10011010", 2).toByte
    val last = Integer.parseInt("00001010", 2).toByte
    val data = Array(prefix, continue, last).iterator
    val dec = Decoder()
    val result = dec.decodeInt(data, data.next(), 5)
    assertEquals(result, 1337)
  }

  test("roundtrip encoder and decoder without huffman") {
    val enc = Encoder()
    val dec = Decoder()
    val (key, value) = ("custom-key", "custom-value")
    for indexing <- Encoder.Opt.all do
      val roundtrip =
        dec.decode(enc.encode(key, Seq(value), indexing))
      assertEquals(roundtrip, ArrayBuffer(key -> value))
  }
  test("roundtrip encoder and decoder with huffman") {
    val enc = Encoder()
    val dec = Decoder()
    val (key, value) = ("custom-key", "custom-value")
    for indexing <- Encoder.Opt.all do
      val roundtrip =
        dec.decode(enc.encode(key, Seq(value), indexing, true))
      assertEquals(roundtrip, ArrayBuffer(key -> value))
  }

  test("encoding literal header with indexing") {
    val enc = Encoder()
    val data: Array[Byte] = Array(
      0x40, 0x0a, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x6b, 0x65, 0x79,
      0x0d, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x68, 0x65, 0x61, 0x64,
      0x65, 0x72
    )
    val result =
      enc.encode("custom-key", Seq("custom-header"), Encoder.Opt.ALWAYS)
    assertEquals(result.toSeq, data.toSeq)
  }
  test("decoding literal header with indexing") {
    val dec = Decoder()
    val data: Array[Byte] = Array(
      0x40, 0x0a, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x6b, 0x65, 0x79,
      0x0d, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x68, 0x65, 0x61, 0x64,
      0x65, 0x72
    )
    val result = dec.decode(data)
    assertEquals(new String(data), "@\ncustom-key\rcustom-header")
    assertEquals(result, ArrayBuffer(("custom-key", "custom-header")))
    assertEquals(dec.table.size, 1)
    assertEquals(dec.table.byteSize, 55)
  }
  test("encoding literal header without indexing") {
    val data: Seq[Byte] = Array[Byte](
      0x04, 0x0c, 0x2f, 0x73, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2f, 0x70, 0x61,
      0x74, 0x68
    ).toSeq
    val enc = Encoder()
    val result = enc.encode(":path", Seq("/sample/path"), Encoder.Opt.NONE)
    assertEquals(result.toSeq, data)
  }
  test("decoding literal header without indexing") {
    val data: Array[Byte] = Array(
      0x04, 0x0c, 0x2f, 0x73, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2f, 0x70, 0x61,
      0x74, 0x68
    )
    val dec = Decoder()
    val result = dec.decode(data)
    assertEquals(new String(data), "\u0004\f/sample/path")
    assertEquals(result, ArrayBuffer((":path", "/sample/path")))
    assert(dec.table.isEmpty)
  }
  test("encoding literal header never indexed") {
    val data = Array[Byte](0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77, 0x6f, 0x72,
      0x64, 0x06, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74).toSeq
    val enc = Encoder()
    val result = enc.encode("password", Seq("secret"), Encoder.Opt.NEVER)
    assertEquals(result.toSeq, data)
    assert(enc.table.isEmpty)
  }
  test("decoding literal header never indexed") {
    val data: Array[Byte] = Array(0x10, 0x08, 0x70, 0x61, 0x73, 0x73, 0x77,
      0x6f, 0x72, 0x64, 0x06, 0x73, 0x65, 0x63, 0x72, 0x65, 0x74)
    val dec = Decoder()
    val result = dec.decode(data)
    assertEquals(new String(data), "\u0010\bpassword\u0006secret")
    assertEquals(result, ArrayBuffer(("password", "secret")))
    assert(dec.table.isEmpty)
  }
  test("encoding indexed header field") {
    val data: Array[Byte] = 0x82.bytes()
    val enc = Encoder()
    val result = enc.encode(":method", Seq("GET"), Encoder.Opt.INDEXED)
    assert(enc.table.isEmpty)
  }
  test("decoding indexed header field") {
    val data: Array[Byte] = 0x82.bytes()
    val dec = Decoder()
    val result = dec.decode(data)
    assert(dec.table.isEmpty)
    assertEquals(result, ArrayBuffer((":method", "GET")))
  }

  test("a request without huffman coding") {
    val wellKnownHeaders: Array[Byte] =
      0x82.bytes() ++ 0x86.bytes() ++ 0x84.bytes()
    val data: Array[Byte] =
      wellKnownHeaders ++ Array[Byte](0x41, 0x0f, 0x77, 0x77, 0x77, 0x2e, 0x65,
        0x78, 0x61, 0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d)
    val dec = Decoder()
    val result = dec.decode(data)
    assertEquals(dec.table.size, 1)
    assertEquals(dec.table.byteSize, 57)
    assertEquals(
      result,
      ArrayBuffer(
        ":method" -> "GET",
        ":scheme" -> "http",
        ":path" -> "/",
        ":authority" -> "www.example.com"
      )
    )
  }
  test("3 requests without huffman coding") {
    val dec = Decoder()
    val first: Array[Byte] = Array(
      0x82, 0x86, 0x84, 0x41, 0x0f, 0x77, 0x77, 0x77, 0x2e, 0x65, 0x78, 0x61,
      0x6d, 0x70, 0x6c, 0x65, 0x2e, 0x63, 0x6f, 0x6d
    ).flatMap(_.bytes())
    val result = dec.decode(first)
    assertEquals(
      result,
      ArrayBuffer(
        ":method" -> "GET",
        ":scheme" -> "http",
        ":path" -> "/",
        ":authority" -> "www.example.com"
      )
    )
    assertEquals(dec.table.size, 1)
    assertEquals(dec.table.byteSize, 57)
    assertEquals(dec.indexed(62), ":authority" -> "www.example.com")
    val second = Array(
      0x82, 0x86, 0x84, 0xbe, 0x58, 0x08, 0x6e, 0x6f, 0x2d, 0x63, 0x61, 0x63,
      0x68, 0x65
    ).flatMap(_.bytes())
    val result2 = dec.decode(second)
    assertEquals(
      result2,
      ArrayBuffer(
        ":method" -> "GET",
        ":scheme" -> "http",
        ":path" -> "/",
        ":authority" -> "www.example.com",
        "cache-control" -> "no-cache"
      )
    )
    assertEquals(dec.table.size, 2)
    assertEquals(dec.table.byteSize, 110)
    assertEquals(dec.indexed(62), "cache-control" -> "no-cache")
    assertEquals(dec.indexed(63), ":authority" -> "www.example.com")
    val third = Array(
      0x82, 0x87, 0x85, 0xbf, 0x40, 0x0a, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d,
      0x2d, 0x6b, 0x65, 0x79, 0x0c, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d,
      0x76, 0x61, 0x6c, 0x75, 0x65
    ).flatMap(_.bytes())

    val result3 = dec.decode(third)
    assertEquals(
      result3,
      ArrayBuffer(
        ":method" -> "GET",
        ":scheme" -> "https",
        ":path" -> "/index.html",
        ":authority" -> "www.example.com",
        "custom-key" -> "custom-value"
      )
    )
    assertEquals(dec.table.size, 3)
    assertEquals(dec.table.byteSize, 164)
    assertEquals(dec.indexed(62), ("custom-key", "custom-value"))
    assertEquals(dec.indexed(63), ("cache-control", "no-cache"))
    assertEquals(dec.indexed(64), (":authority", "www.example.com"))
  }
  test("a request with huffman coding") {
    val data: Array[Byte] =
      Array(0x82, 0x86, 0x84, 0x41, 0x8c, 0xf1, 0xe3, 0xc2, 0xe5, 0xf2, 0x3a,
        0x6b, 0xa0, 0xab, 0x90, 0xf4, 0xff).flatMap(_.bytes())
    val dec = Decoder()
    val result = dec.decode(data)
    assertEquals(dec.table.size, 1)
    assertEquals(dec.table.byteSize, 57)
    assertEquals(
      result,
      ArrayBuffer(
        ":method" -> "GET",
        ":scheme" -> "http",
        ":path" -> "/",
        ":authority" -> "www.example.com"
      )
    )
  }

  test("edge integer literal") {
    val data: Array[Byte] = Array.fill(3 + 2 + 127)('.'.toByte)
    data(0) = 0x00
    data(1) = 0x01
    data(2) = 'x'.toByte
    data(3) = 0x7f
    data(4) = 0x00
    val dec = Decoder()
    val result = dec.decode(data)
    assertEquals(result, ArrayBuffer(("x", "." * 127)))
  }
  test("reject padding larger than 7 bits") {
    val data: Array[Byte] = Array(
      0x82, 0x87, 0x84, 0x41, 0x8a, 0x08, 0x9d, 0x5c, 0x0b, 0x81, 0x70, 0xdc,
      0x7c, 0x4f, 0x8b, 0x00, 0x85, 0xf2, 0xb2, 0x4a, 0x84, 0xff, 0x84, 0x49,
      0x50, 0x9f, 0xff
    ).flatMap(_.bytes())
    val dec = Decoder()
    intercept[Exception](dec.decode(data))
  }
  test("reject non EOS padding") {
    val data: Array[Byte] = Array(
      0x82, 0x87, 0x84, 0x41, 0x8a, 0x08, 0x9d, 0x5c, 0x0b, 0x81, 0x70, 0xdc,
      0x7c, 0x4f, 0x8b, 0x00, 0x85, 0xf2, 0xb2, 0x4a, 0x84, 0xff, 0x83, 0x49,
      0x50, 0x90
    ).flatMap(_.bytes())
    val dec = Decoder()
    intercept[Exception](dec.decode(data))
  }
  test("literal header with indexing") {
    val headers = ArrayBuffer("custom-key" -> "custom-header")
    val data = Array[Byte](0x40, 0x0a, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d,
      0x6b, 0x65, 0x79, 0x0d, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x68,
      0x65, 0x61, 0x64, 0x65, 0x72)
    // enc.encode(headers, Indexing::ALWAYS)
    // assert_equal 1, enc.table.size
    // assert_equal 55, enc.table.bytesize
    // assert_equal({"custom-key", "custom-header"}, enc.table[0])
  }

  extension (value: Int)
    private def bytes(): Array[Byte] =
      Array(
        (value >> 24).byteValue,
        (value >> 16).byteValue,
        (value >> 8).byteValue,
        value.byteValue
      ).dropWhile(_ == 0)

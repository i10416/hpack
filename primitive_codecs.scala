package http2.hpack
import http2.hpack.syntax._
import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer

/** A trait to gather int and string decoding functionality */
private[hpack] trait PrimitiveDecoder:

  /** `decodeInt` decodes Int from bits, using n bits from current offset as a
    * prefix. When all the bits are 1, it consumes the trailing bits to decode
    * Int larger than prefix bits until it finds the most significant bit being
    * `0`.
    *
    * ## Example
    *
    * ### Case 1:
    *
    * When int is less than 2^N -1, it fits in the prefix.
    *
    * Suppose n = 5;
    *
    * {{{
    *
    * +-+-+-+-+-+-+-+-+
    * |0|1|0|1|0|0|1|1| <- prefix in the octet(the least 5 bits) are the value encoded.
    * +-+-+-+-+-+-+-+-+
    *
    * }}}
    *
    * ### Case 2:
    *
    * When n(=prefix) is 5 and value is equals to or larger than 2^N - 1, Int is
    * layouted in the following format.
    *
    * {{{
    *
    * +-+-+-+-+-+-+-+-+
    * |0|1|0|1|1|1|1|1| <- prefix in the octet(least 5 bits) are all `1`
    * +-+-+-+-+-+-+-+-+
    * |1|0|1|1|0|1|0|1| <- When the most significant bit is `1`, it implies
    * +-+-+-+-+-+-+-+-+    it needs one or more trailing octets to decode Int.
    * |0|0|0|1|0|0|1|1| <- The most significant bit being `0` means
    * +-+-+-+-+-+-+-+-+    this octet is the last octet.
    *
    * }}}
    *
    * @see
    *   See [[https://datatracker.ietf.org/doc/html/rfc7541#section-5.1]] for
    *   more precise description.
    */
  protected[hpack] def decodeInt(
      iter: Iterator[Byte],
      current: Byte,
      n: Int
  ): Int =
    // extract least n bits from octet(exact 8 bits)
    var int: Int = (current & (0xff >> (8 - n))).toInt
    val n2 = (2 ** n).toInt - 1
    // If an integer cannot fit in a prefix, all bits in prefix are `1`.
    // Therefore, if int < n2 (= 111...111), there is at least one bit that isn't `1`.
    if int < n2 then return int

    // When a prefix is 111...111.
    var m = 0
    var continue = true
    while continue && iter.hasNext do
      val byte = iter.next()
      // 5.1: Integer Representation
      // > The most significant bit of each octet is used as a
      // > continuation flag: its value is set to 1 except for the
      // > last octet in the list.
      int += (byte & `0b01111111`).toInt * ((2 ** m).toInt)
      if (byte & 128) != 128 then continue = false
      m += 7
    int

  protected[hpack] def decodeStr(
      iter: Iterator[Byte]
  ): String =
    val current = iter.next()
    val huffman = current.bit(7) == 1
    val length = decodeInt(iter, current, 7)
    val bytes: Array[Byte] = iter.take(length).toArray
    if huffman then Huffman.INSTANCE.decode(bytes)
    else
      val str = new String(bytes)
      str

/** A trait to gather int and string encoding functionality
  */
private[hpack] trait PrimitiveEncoder:
  /** encode integer into the given `buf`.
    *
    * @param int
    *   an integer to encode
    * @param buf
    *   byte buffer to encode integer into
    * @param n
    *   use the least `n` bits of the prefix octet
    *
    * @param prefix
    *   the prefix octet to encode integer(or part of integer) into
    */
  protected[hpack] def encodeInt(
      int: Int,
      n: Int,
      buf: ArrayBuffer[Byte],
      prefix: Byte = `0b00000000`
  ): Unit =
    var integer = int
    val n2: Int = (2 ** n).toInt - 1
    if integer < n2 then // integer fits in the least n bits of the prefix octet
      buf += ((integer | prefix).toByte)
      return
    // here, all bits in `n2` are `1`
    buf += ((n2.toByte | prefix.toByte).toByte)
    integer -= n2
    while integer >= 128 do
      buf += (((integer % 128) + 128).toByte)
      integer = integer / 128
    buf += (integer.toByte)

  /** encode string into the given `buf`. String is encoded as either literal
    * value or Huffman-encoded value.
    *
    * @param str
    *   string to encode into bytes
    * @param buf
    *   a byte buffer to encode string into
    * @param huffman
    *   enable Huffman encoding. Default is false.
    */
  protected[hpack] def encodeStr(
      str: String,
      buf: ArrayBuffer[Byte],
      huffman: Boolean = false
  ): Unit =
    if huffman then
      val encoded = Huffman.INSTANCE.encode(str)
      encodeInt(encoded.size, 7, buf, `0b10000000`)
      buf ++= (encoded)
    else
      val bytes = str.getBytes().nn
      encodeInt(bytes.size, 7, buf)
      buf ++= (bytes)

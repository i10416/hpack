package http2.hpack

/** utility package to provide convenient syntax for manupulating bytes.
  * @example
  *   ```
  *   import http2.hpack.syntax
  *   val bits = Integer.parseInt("11101",2)
  *   bits.bit(0) // => 1
  *   bits.bit(1) // => 0
  *   val _ : Array[Byte] = 0x82.bytes()
  *   ```
  */
private object syntax:
  /** 128 in signed Integer and unsigned byte,  -128 in signed byte */
  final val `0b10000000`: Byte = 128.toByte
  final val `0b00000000`: Byte = 0.toByte
  final val `0b01111111`: Byte = 127.toByte
  extension (i: Int) def downTo(to: Int) = i.to(to, -1)
  extension (bits: Int)
    /** get a bit value at `position` in the bits, assuming `position` is within
      * the bits.
      */
    inline def bit(position: Int): Int = (bits >> position) & 1
  extension (bytes: Array[Byte])
    /** equivalent to `bytes(position) = (bytes(position)|byte).toByte`
      */
    inline def bitOr(position: Int)(byte: Byte) =
      bytes(position) = (bytes(position) | byte).toByte

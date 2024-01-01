//> using scala "3.3.1"
//> using file "dynamic_table.scala"
//> using file "static_table.scala"
//> using file "huffman.scala"
//> using file "syntax.scala"
//> using file "primitive_codecs.scala"
//> using options "-Yexplicit-nulls"
//> using options "-deprecation"

package http2.hpack

import scala.collection.mutable.ArrayBuffer

import syntax._
import scala.annotation.tailrec
import http2.hpack.$.STATIC_TABLE

/** Decoder performs HPACK decoding.
  *
  * @example
  *   ```
  *   import http2.hpack.*
  *
  *   val decoder = Decoder()
  *   val data: Array[Byte] = Array(-126) // 0x82(0b10000010) in signed decimal representation
  *   val result = decoder.decode(data)
  *   println(result.toList) // => List((":method", "GET"))
  *   ```
  *
  * @see
  *   [[https://datatracker.ietf.org/doc/html/rfc7541#section-2.3.2]] for
  *   dynamic table specification
  */
final class Decoder(
    val table: DynamicTable,
    maxTableSize: Int
) extends PrimitiveDecoder:
  /** decode header fields from bytes and update the dynamic table if necessary
    * @param bytes
    *   bytes containing header fields encoded by HPACK encoder
    * @return
    *   a key-value list of decoded header fields such as `(":authority",
    *   "www.example.com")`
    */
  def decode(
      bytes: Array[Byte],
      headers: ArrayBuffer[(String, String)] = ArrayBuffer.empty
  ) =
    var decodedCommonHeaders = false
    val src = bytes.iterator
    var skipToNext = false
    while src.hasNext do
      var (name, value): (String | Null, String | Null) = (null, null)
      var index: Int | Null = null
      val current = src.next()
      if current.bit(7) == 1 then // 0b1.......  indexed header field
        index = decodeInt(src, current, 7)
        // 6.1
        // > The index value of 0 is not used. It MUST be treated as a decoding error
        // > if found in an indexed header field representation.
        if index == 0 then
          throw new Exception("invalid index: 0") // index starts with 1!
        val pair = indexed(index.nn)
        name = pair._1
        value = pair._2
      else if current.bit(6) == 1 then // literal with incremental indexing
        index = decodeInt(src, current, 6)
        // field can be indexed, but field value is variable.
        name = if index == 0 then decodeStr(src) else indexed(index.nn)._1
        value = decodeStr(src)
        table.add(name.nn, value.nn)
      else if current.bit(5) == 1 then // dynamic table size update
        if decodedCommonHeaders then
          throw new Exception("unexpected dynamic table size update")
        val newSize = decodeInt(src, current, 5)
        // > The new maximum size MUST be lower than or equal to the limit determined by
        // >  the protocol using HPACK. A value that exceeds this limit MUST be treated
        // > as a decoding error.
        if newSize > maxTableSize then
          throw new Exception(
            "dynamic table size update is larger than SETTINGS_HEADER_TABLE_SIZE"
          )
        table.resize(newSize)
        skipToNext = true
      else if current.bit(4) == 1 then // 0001....  literal never indexed
        index = decodeInt(src, current, 4)
        name = if index == 0 then decodeStr(src) else indexed(index.nn)._1
        value = decodeStr(src)
        // > 6.2.2
        // > A literal header field without indexing representation results
        // > in appending a header field to the decoded header list without
        // > altering the dynamic table.
      else // 0000....  literal without indexing

        index = decodeInt(src, current, 4)
        name = if index == 0 then decodeStr(src) else indexed(index.nn)._1
        value = decodeStr(src)
      if !skipToNext then
        decodedCommonHeaders = 0 < index.nn && index.nn < $.STATIC_TABLE_SIZE
        headers += ((name.nn, value.nn))
      skipToNext = false

    headers

  /** get indexed header fields from static table and dynamic table. The static
    * table is followed by the dynamic table. This may raise an exception when
    * the index is larger than total table size.
    * @param index
    *   index in tables
    *
    * @see
    *   [[https://datatracker.ietf.org/doc/html/rfc7541#section-2.3.3]]
    */
  protected[hpack] def indexed(index: Int): (String, String) =
    if 0 < index && index < $.STATIC_TABLE_SIZE then
      return $.STATIC_TABLE(index - 1)
    val header = table.get(index - $.STATIC_TABLE_SIZE - 1)
    if header.isDefined then return header.get
    throw new Exception(s"invalid index: ${index}")

object Decoder:
  /** create Decoder with maxTableSize.
    *
    * @param maxTableSize
    *   Optional max dynamic table size (in bits) required mainly for security.
    *   Default is 4096.
    */
  def apply(maxTableSize: Int = 4096) =
    val table = DynamicTable(maxTableSize)
    new Decoder(table, maxTableSize)

final class Encoder(val table: DynamicTable) extends PrimitiveEncoder:
  /** encode a seq of headers into bytes using the given indexing option and
    * huffman encoding flag. Headers are inserted in the order where headers
    * with `:` prefix come first.
    *
    * @param headers
    *   a list of header key-value pair
    * @param indexing
    *   indexing option
    * @param huffman
    *   enable huffman encoding. Default is false.
    */
  def encodeSeq(
      headers: Seq[(String, String)],
      indexing: Encoder.Opt,
      huffman: Boolean = false,
      buf: ArrayBuffer[Byte] = ArrayBuffer.empty
  ): Array[Byte] =
    val headerList = headers sortWith putSpecialHeadersFirst
    headerList
      .flatMap: (name, value) =>
        encode(name, Seq(value), indexing, huffman)
      .toArray

  /** encode header values with name into bytes using the given indexing option
    * and huffman encoding flag.
    *
    * @param name
    *   a key of header values
    * @param values
    *   a list of header values associated with the `name`
    * @param huffman
    *   enable huffman encoding. Default is false.
    */
  def encode(
      name: String,
      values: Seq[String],
      indexing: Encoder.Opt,
      huffman: Boolean = false,
      buf: ArrayBuffer[Byte] = ArrayBuffer.empty
  ): Array[Byte] =
    for value <- values do
      indexed(name, value) match
        case Some((index, null)) =>
          if indexing == Encoder.Opt.ALWAYS then
            encodeInt(index, 6, buf, Encoder.Opt.ALWAYS.byte)
            encodeStr(value, buf, huffman)
            table.add(name, value)
          else
            encodeInt(index, 6, buf, Encoder.Opt.NONE.byte)
            encodeStr(value, buf, huffman)
        case Some((index, value)) =>
          encodeInt(index, 7, buf, Encoder.Opt.INDEXED.byte)
        case None =>
          indexing match
            case always @ Encoder.Opt.ALWAYS =>
              table.add(name, value)
              buf += always.byte
            case never @ Encoder.Opt.NEVER =>
              buf += never.byte
            case _ =>
              buf += Encoder.Opt.NONE.byte
          encodeStr(name, buf, huffman)
          encodeStr(value, buf, huffman)
    buf.toArray

  /** lookup index in static table and dynamic table by name and value pair.
    * @param name
    *   a name of header key
    * @param value
    *   value associated with the `name`
    *
    * @return
    *   index in the table and value if it exists
    */
  protected def indexed(
      name: String,
      value: String
  ): Option[(Int, String | Null)] =
    @tailrec
    def go(
        table: Seq[(String, String)],
        rowIdx: Int = 0
        // i: Int | Null = null
    ): Option[(Int, String | Null)] =
      if rowIdx >= table.size then None
      else
        table(rowIdx) match
          case (`name`, `value`) => Some((rowIdx + 1, value))
          case (`name`, v) =>
            Some((rowIdx + 1, null))
          case _ => go(table, rowIdx + 1)
    go(STATIC_TABLE) match
      case Some(headerWithIdx) => Some(headerWithIdx)
      case None =>
        table.table.zipWithIndex.collectFirst:
          case ((`name`, `value`), i) =>
            (i + $.STATIC_TABLE_SIZE + 1, value)
  private[hpack] def putSpecialHeadersFirst(
      one: (String, String),
      another: (String, String)
  ): Boolean =
    (one, another) match
      case ((s":$_", _), (s":$_", _)) => false
      case ((s":$_", _), (_, _))      => true
      case ((_, _), (s":$_", _))      => false
      case _                          => false

object Encoder:
  opaque type Opt = 128 | 64 | 16 | 0
  object Opt:
    extension (o: Opt) def byte: Byte = o.toByte
    val INDEXED: Opt & 128 = 128
    val ALWAYS: Opt & 64 = 64
    val NEVER: Opt & 16 = 16
    val NONE: Opt & 0 = 0
    val all = Set(INDEXED, ALWAYS, NEVER, NONE)

  /** create Decoder with maxTableSize.
    *
    * @param maxTableSize
    *   Optional max dynamic table size (in bits) required mainly for security.
    *   Default is 4096.
    */
  def apply(maxTableSize: Int = 4096) =
    val table = DynamicTable(maxTableSize)
    new Encoder(table)

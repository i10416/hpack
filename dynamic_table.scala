package http2.hpack
import scala.collection.mutable.ArrayDeque

/** Internal state of HPACK compression codec to cache header fields. Table has
  * a maximum size and table entry is preserved on FIFO basis.
  */
private[hpack] final class DynamicTable(
    var maximum: Int,
    var byteSize: Int = 0,
    private[hpack] val table: ArrayDeque[(String, String)] = ArrayDeque.empty
):
  /** Add new header field entry into table. If table size exceeds the max size,
    * the oldest entry is removed.
    */
  def add(name: String, value: String): Unit =
    val header = (name, value)
    table.prepend(header)
    byteSize += count(header)
    cleanup()

  /** get header field entry by index. This may raise an exception if index is
    * out of bound.
    *
    * @return
    *   a header field name and value
    */
  def apply(index: Int): (String, String) = table(index)

  /** get header field entry by index.
    *
    * @return
    *   a header field name and value
    */
  def get(index: Int): Option[(String, String)] =
    if index < table.length then Some(table(index)) else None
  def size: Int = table.size
  def isEmpty: Boolean = table.isEmpty

  /** Resize table to `max`. If table size is larger than `max`, the oldest
    * elements are removed.
    */
  def resize(max: Int) =
    this.maximum = max
    cleanup()
  private def cleanup(): Unit =
    // > 4.3 Entry Eviction When Dynamic Table Size Changes
    // > Whenever the maximum size for the dynamic table is reduced,
    // > entries are evicted from the end of the dynamic table until
    // > the size of the dynamic table is less than or equal to the maximum size.
    while byteSize > maximum do byteSize -= count(table.removeLast())

  /** calculate the size of an entry
    *
    * @return
    *   header field entry length (in octets)
    */
  private def count(header: (String, String)): Int =
    // > 4.1 Calculating Table Size
    // > The size of an entry is the sum of its name's
    // > length in octets (as defined in Section 5.2), its value's length in octets, and 32.
    header._1.getBytes().nn.size + header._2.getBytes().nn.size + 32

private[hpack] object DynamicTable:
  def apply(maximum: Int) = new DynamicTable(maximum)

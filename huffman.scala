package http2.hpack
import java.util.ArrayDeque
import scala.collection.mutable.ArrayBuffer

import syntax._
import http2.hpack.Huffman.Node

/** Huffman codec for HTTP headers. Value(byte) are padded with the most
  * significant bit of the EOS symbol so that it appropriately aligns in an
  * octet.
  */
private[hpack] class Huffman private (
    private val tree: Huffman.Node,
    private val table: Array[(Byte, Int, Int)]
):
  /** encode string into bytes
    */
  def encode(string: String): Array[Byte] =
    val strBytes = string.getBytes().nn
    var bytes: Array[Byte] = Array.ofDim(strBytes.size)
    var offset = 0
    var k: Int = 0
    strBytes
      .foreach: byte =>
        val (_, binary, len) = table((0xff & byte))
        (len - 1)
          .to(0, -1)
          .foreach: i =>
            val j = offset % 8
            if j == 0 then k = offset / 8
            if binary.bit(i) == 1 then bytes.bitOr(k)((128 >> j).toByte)
            offset += 1
    val count = (offset / 8.0).ceil.toInt
    if offset % 8 != 0 then
      // pad the last byte with MOS of EOS symbol
      bytes.bitOr(count - 1)((0xff >> (offset % 8)).toByte)
    bytes.slice(0, count)

  /** decode string from bytes
    */
  def decode(bytes: Array[Byte]): String =
    val buf = ArrayBuffer[Byte]()
    var node: Node | Null = tree
    var eosPadding = true
    bytes.foreach: byte =>
      var byteHasValue = false
      eosPadding = true
      7.to(0, -1)
        .foreach: i =>
          if byte.bit(i) == 1 then node = node.nn.right
          else
            node = node.nn.left
            eosPadding = false
          end if
          if node == null then throw new Exception("node is null!")
          val value = node.nn.value
          if value != null then
            buf += value.nn
            node = tree
            byteHasValue = true
            eosPadding = true
      if !byteHasValue then
        throw new Exception("Huffman string padding is larger than 7-bits")
    if !eosPadding then
      throw new Exception("huffman string padding must use MSB of EOS symbol")
    String(buf.toArray)
private[hpack] object Huffman:
  private class Node(
      var left: Node | Null = null,
      var right: Node | Null = null,
      var value: Byte | Null = null
  ):
    def isLeaf: Boolean = left == null && right == null
    def add(binary: Int, len: Int, value: Byte): Node =
      var node: Node = this
      (len - 1)
        .to(0, -1)
        .foreach: i =>
          if binary.bit(i) == 1 then
            if node.right == null then node.right = Node()
            node = node.right.nn
          else
            if node.left == null then node.left = Node()
            node = node.left.nn
          end if
      node.value = value
      node
    end add
  end Node

  def apply(table: Array[(Byte, Int, Int)]): Huffman =
    val tree = Node()
    table.foreach: (value, binary, len) =>
      tree.add(binary, len, value)
    new Huffman(tree = tree, table = table)

  val INSTANCE = Huffman(
    Array(
      (0.byteValue, Integer.parseInt("1111111111000", 2), 13),
      (1.byteValue, Integer.parseInt("11111111111111111011000", 2), 23),
      (2.byteValue, Integer.parseInt("1111111111111111111111100010", 2), 28),
      (3.byteValue, Integer.parseInt("1111111111111111111111100011", 2), 28),
      (4.byteValue, Integer.parseInt("1111111111111111111111100100", 2), 28),
      (5.byteValue, Integer.parseInt("1111111111111111111111100101", 2), 28),
      (6.byteValue, Integer.parseInt("1111111111111111111111100110", 2), 28),
      (7.byteValue, Integer.parseInt("1111111111111111111111100111", 2), 28),
      (8.byteValue, Integer.parseInt("1111111111111111111111101000", 2), 28),
      (9.byteValue, Integer.parseInt("111111111111111111101010", 2), 24),
      (10.byteValue, Integer.parseInt("111111111111111111111111111100", 2), 30),
      (11.byteValue, Integer.parseInt("1111111111111111111111101001", 2), 28),
      (12.byteValue, Integer.parseInt("1111111111111111111111101010", 2), 28),
      (13.byteValue, Integer.parseInt("111111111111111111111111111101", 2), 30),
      (14.byteValue, Integer.parseInt("1111111111111111111111101011", 2), 28),
      (15.byteValue, Integer.parseInt("1111111111111111111111101100", 2), 28),
      (16.byteValue, Integer.parseInt("1111111111111111111111101101", 2), 28),
      (17.byteValue, Integer.parseInt("1111111111111111111111101110", 2), 28),
      (18.byteValue, Integer.parseInt("1111111111111111111111101111", 2), 28),
      (19.byteValue, Integer.parseInt("1111111111111111111111110000", 2), 28),
      (20.byteValue, Integer.parseInt("1111111111111111111111110001", 2), 28),
      (21.byteValue, Integer.parseInt("1111111111111111111111110010", 2), 28),
      (22.byteValue, Integer.parseInt("111111111111111111111111111110", 2), 30),
      (23.byteValue, Integer.parseInt("1111111111111111111111110011", 2), 28),
      (24.byteValue, Integer.parseInt("1111111111111111111111110100", 2), 28),
      (25.byteValue, Integer.parseInt("1111111111111111111111110101", 2), 28),
      (26.byteValue, Integer.parseInt("1111111111111111111111110110", 2), 28),
      (27.byteValue, Integer.parseInt("1111111111111111111111110111", 2), 28),
      (28.byteValue, Integer.parseInt("1111111111111111111111111000", 2), 28),
      (29.byteValue, Integer.parseInt("1111111111111111111111111001", 2), 28),
      (30.byteValue, Integer.parseInt("1111111111111111111111111010", 2), 28),
      (31.byteValue, Integer.parseInt("1111111111111111111111111011", 2), 28),
      (32.byteValue, Integer.parseInt("010100", 2), 6),
      (33.byteValue, Integer.parseInt("1111111000", 2), 10),
      (34.byteValue, Integer.parseInt("1111111001", 2), 10),
      (35.byteValue, Integer.parseInt("111111111010", 2), 12),
      (36.byteValue, Integer.parseInt("1111111111001", 2), 13),
      (37.byteValue, Integer.parseInt("010101", 2), 6),
      (38.byteValue, Integer.parseInt("11111000", 2), 8),
      (39.byteValue, Integer.parseInt("11111111010", 2), 11),
      (40.byteValue, Integer.parseInt("1111111010", 2), 10),
      (41.byteValue, Integer.parseInt("1111111011", 2), 10),
      (42.byteValue, Integer.parseInt("11111001", 2), 8),
      (43.byteValue, Integer.parseInt("11111111011", 2), 11),
      (44.byteValue, Integer.parseInt("11111010", 2), 8),
      (45.byteValue, Integer.parseInt("010110", 2), 6),
      (46.byteValue, Integer.parseInt("010111", 2), 6),
      (47.byteValue, Integer.parseInt("011000", 2), 6),
      (48.byteValue, Integer.parseInt("00000", 2), 5),
      (49.byteValue, Integer.parseInt("00001", 2), 5),
      (50.byteValue, Integer.parseInt("00010", 2), 5),
      (51.byteValue, Integer.parseInt("011001", 2), 6),
      (52.byteValue, Integer.parseInt("011010", 2), 6),
      (53.byteValue, Integer.parseInt("011011", 2), 6),
      (54.byteValue, Integer.parseInt("011100", 2), 6),
      (55.byteValue, Integer.parseInt("011101", 2), 6),
      (56.byteValue, Integer.parseInt("011110", 2), 6),
      (57.byteValue, Integer.parseInt("011111", 2), 6),
      (58.byteValue, Integer.parseInt("1011100", 2), 7),
      (59.byteValue, Integer.parseInt("11111011", 2), 8),
      (60.byteValue, Integer.parseInt("111111111111100", 2), 15),
      (61.byteValue, Integer.parseInt("100000", 2), 6),
      (62.byteValue, Integer.parseInt("111111111011", 2), 12),
      (63.byteValue, Integer.parseInt("1111111100", 2), 10),
      (64.byteValue, Integer.parseInt("1111111111010", 2), 13),
      (65.byteValue, Integer.parseInt("100001", 2), 6),
      (66.byteValue, Integer.parseInt("1011101", 2), 7),
      (67.byteValue, Integer.parseInt("1011110", 2), 7),
      (68.byteValue, Integer.parseInt("1011111", 2), 7),
      (69.byteValue, Integer.parseInt("1100000", 2), 7),
      (70.byteValue, Integer.parseInt("1100001", 2), 7),
      (71.byteValue, Integer.parseInt("1100010", 2), 7),
      (72.byteValue, Integer.parseInt("1100011", 2), 7),
      (73.byteValue, Integer.parseInt("1100100", 2), 7),
      (74.byteValue, Integer.parseInt("1100101", 2), 7),
      (75.byteValue, Integer.parseInt("1100110", 2), 7),
      (76.byteValue, Integer.parseInt("1100111", 2), 7),
      (77.byteValue, Integer.parseInt("1101000", 2), 7),
      (78.byteValue, Integer.parseInt("1101001", 2), 7),
      (79.byteValue, Integer.parseInt("1101010", 2), 7),
      (80.byteValue, Integer.parseInt("1101011", 2), 7),
      (81.byteValue, Integer.parseInt("1101100", 2), 7),
      (82.byteValue, Integer.parseInt("1101101", 2), 7),
      (83.byteValue, Integer.parseInt("1101110", 2), 7),
      (84.byteValue, Integer.parseInt("1101111", 2), 7),
      (85.byteValue, Integer.parseInt("1110000", 2), 7),
      (86.byteValue, Integer.parseInt("1110001", 2), 7),
      (87.byteValue, Integer.parseInt("1110010", 2), 7),
      (88.byteValue, Integer.parseInt("11111100", 2), 8),
      (89.byteValue, Integer.parseInt("1110011", 2), 7),
      (90.byteValue, Integer.parseInt("11111101", 2), 8),
      (91.byteValue, Integer.parseInt("1111111111011", 2), 13),
      (92.byteValue, Integer.parseInt("1111111111111110000", 2), 19),
      (93.byteValue, Integer.parseInt("1111111111100", 2), 13),
      (94.byteValue, Integer.parseInt("11111111111100", 2), 14),
      (95.byteValue, Integer.parseInt("100010", 2), 6),
      (96.byteValue, Integer.parseInt("111111111111101", 2), 15),
      (97.byteValue, Integer.parseInt("00011", 2), 5),
      (98.byteValue, Integer.parseInt("100011", 2), 6),
      (99.byteValue, Integer.parseInt("00100", 2), 5),
      (100.byteValue, Integer.parseInt("100100", 2), 6),
      (101.byteValue, Integer.parseInt("00101", 2), 5),
      (102.byteValue, Integer.parseInt("100101", 2), 6),
      (103.byteValue, Integer.parseInt("100110", 2), 6),
      (104.byteValue, Integer.parseInt("100111", 2), 6),
      (105.byteValue, Integer.parseInt("00110", 2), 5),
      (106.byteValue, Integer.parseInt("1110100", 2), 7),
      (107.byteValue, Integer.parseInt("1110101", 2), 7),
      (108.byteValue, Integer.parseInt("101000", 2), 6),
      (109.byteValue, Integer.parseInt("101001", 2), 6),
      (110.byteValue, Integer.parseInt("101010", 2), 6),
      (111.byteValue, Integer.parseInt("00111", 2), 5),
      (112.byteValue, Integer.parseInt("101011", 2), 6),
      (113.byteValue, Integer.parseInt("1110110", 2), 7),
      (114.byteValue, Integer.parseInt("101100", 2), 6),
      (115.byteValue, Integer.parseInt("01000", 2), 5),
      (116.byteValue, Integer.parseInt("01001", 2), 5),
      (117.byteValue, Integer.parseInt("101101", 2), 6),
      (118.byteValue, Integer.parseInt("1110111", 2), 7),
      (119.byteValue, Integer.parseInt("1111000", 2), 7),
      (120.byteValue, Integer.parseInt("1111001", 2), 7),
      (121.byteValue, Integer.parseInt("1111010", 2), 7),
      (122.byteValue, Integer.parseInt("1111011", 2), 7),
      (123.byteValue, Integer.parseInt("111111111111110", 2), 15),
      (124.byteValue, Integer.parseInt("11111111100", 2), 11),
      (125.byteValue, Integer.parseInt("11111111111101", 2), 14),
      (126.byteValue, Integer.parseInt("1111111111101", 2), 13),
      (127.byteValue, Integer.parseInt("1111111111111111111111111100", 2), 28),
      (128.byteValue, Integer.parseInt("11111111111111100110", 2), 20),
      (129.byteValue, Integer.parseInt("1111111111111111010010", 2), 22),
      (130.byteValue, Integer.parseInt("11111111111111100111", 2), 20),
      (131.byteValue, Integer.parseInt("11111111111111101000", 2), 20),
      (132.byteValue, Integer.parseInt("1111111111111111010011", 2), 22),
      (133.byteValue, Integer.parseInt("1111111111111111010100", 2), 22),
      (134.byteValue, Integer.parseInt("1111111111111111010101", 2), 22),
      (135.byteValue, Integer.parseInt("11111111111111111011001", 2), 23),
      (136.byteValue, Integer.parseInt("1111111111111111010110", 2), 22),
      (137.byteValue, Integer.parseInt("11111111111111111011010", 2), 23),
      (138.byteValue, Integer.parseInt("11111111111111111011011", 2), 23),
      (139.byteValue, Integer.parseInt("11111111111111111011100", 2), 23),
      (140.byteValue, Integer.parseInt("11111111111111111011101", 2), 23),
      (141.byteValue, Integer.parseInt("11111111111111111011110", 2), 23),
      (142.byteValue, Integer.parseInt("111111111111111111101011", 2), 24),
      (143.byteValue, Integer.parseInt("11111111111111111011111", 2), 23),
      (144.byteValue, Integer.parseInt("111111111111111111101100", 2), 24),
      (145.byteValue, Integer.parseInt("111111111111111111101101", 2), 24),
      (146.byteValue, Integer.parseInt("1111111111111111010111", 2), 22),
      (147.byteValue, Integer.parseInt("11111111111111111100000", 2), 23),
      (148.byteValue, Integer.parseInt("111111111111111111101110", 2), 24),
      (149.byteValue, Integer.parseInt("11111111111111111100001", 2), 23),
      (150.byteValue, Integer.parseInt("11111111111111111100010", 2), 23),
      (151.byteValue, Integer.parseInt("11111111111111111100011", 2), 23),
      (152.byteValue, Integer.parseInt("11111111111111111100100", 2), 23),
      (153.byteValue, Integer.parseInt("111111111111111011100", 2), 21),
      (154.byteValue, Integer.parseInt("1111111111111111011000", 2), 22),
      (155.byteValue, Integer.parseInt("11111111111111111100101", 2), 23),
      (156.byteValue, Integer.parseInt("1111111111111111011001", 2), 22),
      (157.byteValue, Integer.parseInt("11111111111111111100110", 2), 23),
      (158.byteValue, Integer.parseInt("11111111111111111100111", 2), 23),
      (159.byteValue, Integer.parseInt("111111111111111111101111", 2), 24),
      (160.byteValue, Integer.parseInt("1111111111111111011010", 2), 22),
      (161.byteValue, Integer.parseInt("111111111111111011101", 2), 21),
      (162.byteValue, Integer.parseInt("11111111111111101001", 2), 20),
      (163.byteValue, Integer.parseInt("1111111111111111011011", 2), 22),
      (164.byteValue, Integer.parseInt("1111111111111111011100", 2), 22),
      (165.byteValue, Integer.parseInt("11111111111111111101000", 2), 23),
      (166.byteValue, Integer.parseInt("11111111111111111101001", 2), 23),
      (167.byteValue, Integer.parseInt("111111111111111011110", 2), 21),
      (168.byteValue, Integer.parseInt("11111111111111111101010", 2), 23),
      (169.byteValue, Integer.parseInt("1111111111111111011101", 2), 22),
      (170.byteValue, Integer.parseInt("1111111111111111011110", 2), 22),
      (171.byteValue, Integer.parseInt("111111111111111111110000", 2), 24),
      (172.byteValue, Integer.parseInt("111111111111111011111", 2), 21),
      (173.byteValue, Integer.parseInt("1111111111111111011111", 2), 22),
      (174.byteValue, Integer.parseInt("11111111111111111101011", 2), 23),
      (175.byteValue, Integer.parseInt("11111111111111111101100", 2), 23),
      (176.byteValue, Integer.parseInt("111111111111111100000", 2), 21),
      (177.byteValue, Integer.parseInt("111111111111111100001", 2), 21),
      (178.byteValue, Integer.parseInt("1111111111111111100000", 2), 22),
      (179.byteValue, Integer.parseInt("111111111111111100010", 2), 21),
      (180.byteValue, Integer.parseInt("11111111111111111101101", 2), 23),
      (181.byteValue, Integer.parseInt("1111111111111111100001", 2), 22),
      (182.byteValue, Integer.parseInt("11111111111111111101110", 2), 23),
      (183.byteValue, Integer.parseInt("11111111111111111101111", 2), 23),
      (184.byteValue, Integer.parseInt("11111111111111101010", 2), 20),
      (185.byteValue, Integer.parseInt("1111111111111111100010", 2), 22),
      (186.byteValue, Integer.parseInt("1111111111111111100011", 2), 22),
      (187.byteValue, Integer.parseInt("1111111111111111100100", 2), 22),
      (188.byteValue, Integer.parseInt("11111111111111111110000", 2), 23),
      (189.byteValue, Integer.parseInt("1111111111111111100101", 2), 22),
      (190.byteValue, Integer.parseInt("1111111111111111100110", 2), 22),
      (191.byteValue, Integer.parseInt("11111111111111111110001", 2), 23),
      (192.byteValue, Integer.parseInt("11111111111111111111100000", 2), 26),
      (193.byteValue, Integer.parseInt("11111111111111111111100001", 2), 26),
      (194.byteValue, Integer.parseInt("11111111111111101011", 2), 20),
      (195.byteValue, Integer.parseInt("1111111111111110001", 2), 19),
      (196.byteValue, Integer.parseInt("1111111111111111100111", 2), 22),
      (197.byteValue, Integer.parseInt("11111111111111111110010", 2), 23),
      (198.byteValue, Integer.parseInt("1111111111111111101000", 2), 22),
      (199.byteValue, Integer.parseInt("1111111111111111111101100", 2), 25),
      (200.byteValue, Integer.parseInt("11111111111111111111100010", 2), 26),
      (201.byteValue, Integer.parseInt("11111111111111111111100011", 2), 26),
      (202.byteValue, Integer.parseInt("11111111111111111111100100", 2), 26),
      (203.byteValue, Integer.parseInt("111111111111111111111011110", 2), 27),
      (204.byteValue, Integer.parseInt("111111111111111111111011111", 2), 27),
      (205.byteValue, Integer.parseInt("11111111111111111111100101", 2), 26),
      (206.byteValue, Integer.parseInt("111111111111111111110001", 2), 24),
      (207.byteValue, Integer.parseInt("1111111111111111111101101", 2), 25),
      (208.byteValue, Integer.parseInt("1111111111111110010", 2), 19),
      (209.byteValue, Integer.parseInt("111111111111111100011", 2), 21),
      (210.byteValue, Integer.parseInt("11111111111111111111100110", 2), 26),
      (211.byteValue, Integer.parseInt("111111111111111111111100000", 2), 27),
      (212.byteValue, Integer.parseInt("111111111111111111111100001", 2), 27),
      (213.byteValue, Integer.parseInt("11111111111111111111100111", 2), 26),
      (214.byteValue, Integer.parseInt("111111111111111111111100010", 2), 27),
      (215.byteValue, Integer.parseInt("111111111111111111110010", 2), 24),
      (216.byteValue, Integer.parseInt("111111111111111100100", 2), 21),
      (217.byteValue, Integer.parseInt("111111111111111100101", 2), 21),
      (218.byteValue, Integer.parseInt("11111111111111111111101000", 2), 26),
      (219.byteValue, Integer.parseInt("11111111111111111111101001", 2), 26),
      (220.byteValue, Integer.parseInt("1111111111111111111111111101", 2), 28),
      (221.byteValue, Integer.parseInt("111111111111111111111100011", 2), 27),
      (222.byteValue, Integer.parseInt("111111111111111111111100100", 2), 27),
      (223.byteValue, Integer.parseInt("111111111111111111111100101", 2), 27),
      (224.byteValue, Integer.parseInt("11111111111111101100", 2), 20),
      (225.byteValue, Integer.parseInt("111111111111111111110011", 2), 24),
      (226.byteValue, Integer.parseInt("11111111111111101101", 2), 20),
      (227.byteValue, Integer.parseInt("111111111111111100110", 2), 21),
      (228.byteValue, Integer.parseInt("1111111111111111101001", 2), 22),
      (229.byteValue, Integer.parseInt("111111111111111100111", 2), 21),
      (230.byteValue, Integer.parseInt("111111111111111101000", 2), 21),
      (231.byteValue, Integer.parseInt("11111111111111111110011", 2), 23),
      (232.byteValue, Integer.parseInt("1111111111111111101010", 2), 22),
      (233.byteValue, Integer.parseInt("1111111111111111101011", 2), 22),
      (234.byteValue, Integer.parseInt("1111111111111111111101110", 2), 25),
      (235.byteValue, Integer.parseInt("1111111111111111111101111", 2), 25),
      (236.byteValue, Integer.parseInt("111111111111111111110100", 2), 24),
      (237.byteValue, Integer.parseInt("111111111111111111110101", 2), 24),
      (238.byteValue, Integer.parseInt("11111111111111111111101010", 2), 26),
      (239.byteValue, Integer.parseInt("11111111111111111110100", 2), 23),
      (240.byteValue, Integer.parseInt("11111111111111111111101011", 2), 26),
      (241.byteValue, Integer.parseInt("111111111111111111111100110", 2), 27),
      (242.byteValue, Integer.parseInt("11111111111111111111101100", 2), 26),
      (243.byteValue, Integer.parseInt("11111111111111111111101101", 2), 26),
      (244.byteValue, Integer.parseInt("111111111111111111111100111", 2), 27),
      (245.byteValue, Integer.parseInt("111111111111111111111101000", 2), 27),
      (246.byteValue, Integer.parseInt("111111111111111111111101001", 2), 27),
      (247.byteValue, Integer.parseInt("111111111111111111111101010", 2), 27),
      (248.byteValue, Integer.parseInt("111111111111111111111101011", 2), 27),
      (249.byteValue, Integer.parseInt("1111111111111111111111111110", 2), 28),
      (250.byteValue, Integer.parseInt("111111111111111111111101100", 2), 27),
      (251.byteValue, Integer.parseInt("111111111111111111111101101", 2), 27),
      (252.byteValue, Integer.parseInt("111111111111111111111101110", 2), 27),
      (253.byteValue, Integer.parseInt("111111111111111111111101111", 2), 27),
      (254.byteValue, Integer.parseInt("111111111111111111111110000", 2), 27),
      (255.byteValue, Integer.parseInt("11111111111111111111101110", 2), 26)
    )
  )

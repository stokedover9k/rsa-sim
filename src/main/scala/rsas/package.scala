package object rsas {
  /**
   * Returns the number of bits after all leading zeros of n.
   * @param n Number to get number of significant bits for.
   * @return Number of significant bits in n.
   */
  def numberOfSignificantBits(n: Int): Int = {
    def loop(x: Int, count: Int): Int =
      if (x == 0)
        count
      else
        loop(x >> 1, count + 1)

    loop(n, 0)
  }

  /**
   * Convert a byte into a bit string padded with zeroes to length 8.
   * @param byte Bytes to convert.
   * @return String of characters 0 and 1.
   */
  def byte2PaddedBitString(byte: Byte): String =
    String.format("%8s", (byte & 0xff).toBinaryString).replace(' ', '0')

  /**
   * Convert an integer into a bit string padded with zeroes to length 32.
   * @param int Integer to convert.
   * @return String of characters 0 and 1.
   */
  def int2PaddedBitString(int: Int): String =
    String.format("%32s", int.toBinaryString).replace(' ', '0')

  /**
   * Converts a sequence of bytes into space separated bit strings.
   * @param bytes Bytes to convert.
   * @return Space separate bit strings.
   */
  def bytes2BitString(bytes: Seq[Byte]): String =
    bytes.map(byte2PaddedBitString).mkString(" ")

  /**
   * Converts a big-endian sequence of bytes (of length 4) into an integer.
   * @param bytes Bytes to convert.
   * @return Integer.
   * @throws IllegalArgumentException If sequence is not of length 4.
   */
  def bytes2Int(bytes: Seq[Byte]): Int =
    if (bytes.length != 4)
      throw new IllegalArgumentException
    else
      ((bytes(0) & 0xff) << 24) + ((bytes(1) & 0xff) << 16) +
        ((bytes(2) & 0xff) << 8) + (bytes(3) & 0xff)

  /**
   * Converts an integer to a big-endian sequence of bytes.
   * @param n Integer to convert to bytes.
   * @return Sequence of bytes.
   */
  def int2Bytes(n: Int): Seq[Byte] =
    Seq(((n >> 24) & 0xff).toByte, ((n >> 16) & 0xff).toByte,
      ((n >> 8) & 0xff).toByte, (n & 0xff).toByte)
}

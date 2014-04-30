package rsas

case class Certificate private(bytes: Seq[Byte]) {

  import Certificate._

  private def getField[T](start: Int, length: Int,
                          formatter: Seq[Byte] => T): T = {
    formatter(bytes slice(start, start + length))
  }

  def getName: String =
    getField(BYTE_INDEX_OF_NAME, BYTE_LENGTH_OF_NAME, stringFormatter)

  def getN: Int =
    getField(BYTE_INDEX_OF_N, BYTE_LENGTH_OF_N, intFormatter)

  def getE: Int =
    getField(BYTE_INDEX_OF_E, BYTE_LENGTH_OF_E, intFormatter)
}

object Certificate {

  private val stringFormatter: Seq[Byte] => String =
    bytes => new String(bytes map (_.toChar) toArray)

  private val intFormatter: Seq[Byte] => Int = bytes => {
    if (bytes.length != 4)
      throw new IllegalArgumentException
    else
      ((bytes(0) & 0xff) << 24) + ((bytes(1) & 0xff) << 16) +
        ((bytes(2) & 0xff) << 8) + (bytes(3) & 0xff)
  }

  val BYTE_INDEX_OF_NAME = 0
  val BYTE_LENGTH_OF_NAME = 6
  val BYTE_INDEX_OF_N = 6
  val BYTE_LENGTH_OF_N = 4
  val BYTE_INDEX_OF_E = 10
  val BYTE_LENGTH_OF_E = 4

  def length: Int = BYTE_LENGTH_OF_NAME + BYTE_LENGTH_OF_N + BYTE_LENGTH_OF_E

  def apply(name: String, n: Int, e: Int): Certificate = {
    def intToBytes(n: Int): Seq[Byte] =
      Seq(((n >> 24) & 0xff).toByte, ((n >> 16) & 0xff).toByte,
        ((n >> 8) & 0xff).toByte, (n & 0xff).toByte)

    def nameBytes = name.toCharArray.map(_.toByte).
      padTo(BYTE_LENGTH_OF_NAME, 0.toByte)
    def nBytes = intToBytes(n)
    def eBytes = intToBytes(e)

    val bytes = nameBytes ++ nBytes ++ eBytes

    if (bytes.length != length)
      throw new IllegalArgumentException
    else
      new Certificate(bytes)
  }

}
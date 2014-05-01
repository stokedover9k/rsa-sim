package rsas

/**
 * A certificate value object. This simple certificate is of the form
 * (name, n, e) where name is the name of the entity to whom this certificate
 * belongs, n is the public key modulus, and e is the public key exponent. The
 * byte sequence is 14 bytes: the first 6 bytes are the name of the
 * certificate's owner; the next 4 bytes is the modulus in big endian format;
 * the last 4 bytes is the exponent in the big endian format.
 *
 * Certificate objects have convenient getters to extract each of the fields
 * from the underlying byte sequence.
 *
 * @param bytes Underlying byte sequence representing this certificate.
 */
case class Certificate private(bytes: Seq[Byte]) {

  import Certificate._

  private def getField[T](start: Int, length: Int,
                          formatter: Seq[Byte] => T): T = {
    formatter(bytes slice(start, start + length))
  }

  /**
   * Get the name of the certificate's owner.
   * @return Name of the certificate's owner.
   */
  def getName: Name =
    getField(BYTE_OFFSET_OF_NAME, BYTE_LENGTH_OF_NAME, nameFormatter)

  /**
   * Get the modulus of the public key.
   * @return Modulus of the public key.
   */
  def getN: Int =
    getField(BYTE_OFFSET_OF_N, BYTE_LENGTH_OF_N, intFormatter)

  /**
   * Get the exponent of the public key.
   * @return Exponent of the public key.
   */
  def getE: Int =
    getField(BYTE_OFFSET_OF_E, BYTE_LENGTH_OF_E, intFormatter)
}

/**
 * Companion to certificate value objects. This object contains, among other
 * things, indices and lengths of certificate's fields and a factory method.
 */
object Certificate {

  /**
   * Offset of the name field in the underlying byte sequence.
   */
  val BYTE_OFFSET_OF_NAME = 0

  /**
   * Length of the name field in the underlying byte sequence.
   */
  val BYTE_LENGTH_OF_NAME = 6

  /**
   * Length of the modulus field in the underlying byte sequence.
   */
  val BYTE_OFFSET_OF_N = 6

  /**
   * Length of the modulus field in the underlying byte sequence.
   */
  val BYTE_LENGTH_OF_N = 4

  /**
   * Length of the exponent field in the underlying byte sequence.
   */
  val BYTE_OFFSET_OF_E = 10

  /**
   * Length of the exponent field in the underlying byte sequence.
   */
  val BYTE_LENGTH_OF_E = 4

  /**
   * Total length of the certificate in bytes.
   */
  lazy val length: Int = BYTE_LENGTH_OF_NAME + BYTE_LENGTH_OF_N +
    BYTE_LENGTH_OF_E

  private val stringFormatter: Seq[Byte] => String =
    bytes => new String(bytes map (_.toChar) toArray)

  private val nameFormatter: Seq[Byte] => Name =
    bytes => Name.valueOf(stringFormatter(bytes))

  private val intFormatter: Seq[Byte] => Int = bytes =>
    if (bytes.length != 4)
      throw new IllegalArgumentException
    else
      ((bytes(0) & 0xff) << 24) + ((bytes(1) & 0xff) << 16) +
        ((bytes(2) & 0xff) << 8) + (bytes(3) & 0xff)

  /**
   * Create a certificate object.
   * @param name Name of the certificate's owner.
   * @param n Modulus of the public key.
   * @param e Exponent of the public key.
   * @return New certificate with specified fields.
   */
  def apply(name: Name, n: Int, e: Int): Certificate = {
    def intToBytes(n: Int): Seq[Byte] =
      Seq(((n >> 24) & 0xff).toByte, ((n >> 16) & 0xff).toByte,
        ((n >> 8) & 0xff).toByte, (n & 0xff).toByte)

    def nameBytes = name.string.toCharArray.map(_.toByte).
      padTo(BYTE_LENGTH_OF_NAME, 0.toByte)
    def nBytes = intToBytes(n)
    def eBytes = intToBytes(e)

    val bytes = nameBytes ++ nBytes ++ eBytes

    if (bytes.length != length)
      throw new IllegalArgumentException
    else
      new Certificate(bytes)
  }

  /**
   * Certificate name. The underlying string is
   * [[rsas.Certificate.BYTE_LENGTH_OF_NAME]] characters long.
   * @param string Underlying string.
   */
  case class Name (string: String) {
    require(string.length == BYTE_LENGTH_OF_NAME, "invalid name length")
  }

  /**
   * Companion object for certificate names. Contains an implicit conversion
   * from String and a factory which enforces the length constraint.
   */
  object Name {
    implicit def String2Name(string: String): Name = Name.valueOf(string)

    /**
     * Creates a new certificate name padding the given string up to the needed
     * length [[rsas.Certificate.BYTE_LENGTH_OF_NAME]] characters long).
     * @param string String of at most the allowed length.
     * @return New certificate name object.
     * @throws IllegalArgumentException if string is too long.
     */
    def valueOf(string: String): Name =
      if (string.length < 6)
        Name.valueOf(string.padTo(BYTE_LENGTH_OF_NAME, 0.toChar))
      else if (string.length == 6)
        new Name(string.padTo(BYTE_LENGTH_OF_NAME, 0.toChar))
      else
        throw new IllegalArgumentException
  }

}
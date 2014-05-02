package rsas

import rsas.util.ModMath
import rsas.util.ModMath.ModVal

/**
 * Key Holder who can sign [[rsas.Certificate]] objects.
 * @param n Modulus of public key.
 * @param e Exponent of public key.
 */
abstract case class KeyHolder private(name: Certificate.Name, n: Int, e: Int)
  extends Attestant[Certificate, Seq[Byte]] {

  /**
   * Certificate of the Key Holder.
   */
  lazy val certificate: Certificate = Certificate(name = name, n = n, e = e)

  /**
   * An attestant for authentication capable of signing data. Data is hashed
   * and then "decrypted".
   * @return Attestant for authentication.
   */
  def auth: Attestant[Seq[Byte], Seq[Byte]]
}

/**
 * Companion to the Key Holder case class.
 * Provides a factory for discrete creation of Trent holding a private key.
 */
object KeyHolder {

  /**
   * Simple hash taking the exclusive-or of the bytes.
   * @param bytes Bytes to hash.
   * @return Hash of the bytes.
   */
  private def hashInt(bytes: Seq[Byte]): Int =
    bytes.foldLeft(0)(_ ^ _.toInt)

  /**
   * Common key holder hash.
   * @param bytes Bytes to hash.
   * @return Has of the bytes.
   */
  def hash(bytes: Seq[Byte]): Seq[Byte] =
    int2Bytes(hashInt(bytes))

  /**
   * Create a new Key Holder with the given public/private key pair.
   * @param name Name of the Key Holder
   * @param n Modulus of the public/private key pair.
   * @param e Exponent of the public key.
   * @param d Exponent of the private key.
   * @return New Key Holder object with given public/private key pair.
   */
  def apply(name: Certificate.Name, n: Int, e: Int, d: Int): KeyHolder = {
    implicit val mod = ModVal(n)
    implicit def int2ModMath: Int => ModMath = ModMath(_)

    new KeyHolder(name, n = n, e = e) {
      def getSignature(certificate: Certificate): Seq[Byte] = {
        def hashed = hashInt(certificate.bytes)
        def decrypted = ModMath(hashed) **% d
        int2Bytes(decrypted)
      }

      def hash(bytes: Seq[Byte]): Seq[Byte] = KeyHolder.hash(bytes)

      def auth: Attestant[Seq[Byte], Seq[Byte]] =
        new Attestant[Seq[Byte], Seq[Byte]] {
          def getSignature(data: Seq[Byte]): Seq[Byte] = {
            implicit val mod = ModVal(n)
            import ModMath.int2ModMath
            val hashed = bytes2Int(hash(data))
            int2Bytes(hashed **% d)
          }

          def hash(bytes: Seq[Byte]): Seq[Byte] = KeyHolder.hash(bytes)
        }
    }
  }

  /**
   * Create a new Key Holder with the given public/private key pair.
   * @param name Name of Key Holder.
   * @param keyPair Key Holder's key pair.
   * @return New Key Holder object with given public/private key pair.
   */
  def apply(name: Certificate.Name, keyPair: KeyPair): KeyHolder =
    apply(name, n = keyPair.n, e = keyPair.e, d = keyPair.d)
}

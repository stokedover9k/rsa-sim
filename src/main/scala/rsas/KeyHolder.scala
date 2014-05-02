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
}

/**
 * Companion to the Key Holder case class.
 * Provides a factory for discrete creation of Trent holding a private key.
 */
object KeyHolder {

  private def hashInt(bytes: Seq[Byte]): Int =
    bytes.foldLeft(0)(_ ^ _.toInt)

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

      def hash(bytes: Seq[Byte]): Seq[Byte] =
        int2Bytes(hashInt(bytes))
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

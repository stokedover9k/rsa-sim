package rsas

import rsas.util.ModMath
import rsas.util.ModMath.ModVal

/**
 * Trusted Authority who can sign [[rsas.Certificate]] objects.
 * @param n Modulus of public key.
 * @param e Exponent of public key.
 */
abstract case class Trent private(n: Int, e: Int)
  extends Attestant[Certificate, Seq[Byte]]

/**
 * Companion to the Trusted Authority case class.
 * Provides a factory for discrete creation of Trent holding a private key.
 */
object Trent {

  private def hash(bytes: Seq[Byte]): Int =
    bytes.foldLeft(0)(_ ^ _.toInt)

  private def Int2Bytes(n: Int): Seq[Byte] =
    IndexedSeq(((n >> 24) & 0xff).toByte, ((n >> 16) & 0xff).toByte,
      ((n >> 8) & 0xff).toByte, (n & 0xff).toByte)

  /**
   * Create a new Trent with the given public/private key pair.
   * @param n Modulus of the public/private key pair.
   * @param e Exponent of the public key.
   * @param d Exponent of the private key.
   * @return New Trent object with given public/private key pair.
   */
  def apply(n: Int, e: Int, d: Int): Trent = {
    implicit val mod = ModVal(n)
    implicit def int2ModMath: Int => ModMath = ModMath(_)

    new Trent(n = n, e = e) {
      def getSignature: (Certificate) => Seq[Byte] =
        certificate => {

          def hashed = hash(certificate.bytes)
          def decrypted = ModMath(hashed) **% d

          Int2Bytes(decrypted)
        }
    }
  }
}

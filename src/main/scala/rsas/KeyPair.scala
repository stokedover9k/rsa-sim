package rsas

/**
 * Public/private key pair.
 * @param n Common modulus.
 * @param e Public exponent.
 * @param d Private exponent.
 */
case class KeyPair (n: Int, e: Int, d: Int)

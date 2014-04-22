package rsas.util

import scala.util.Random
import rsas.util.ModMath.ModVal
import org.slf4j.LoggerFactory

/**
 * A collection of functions dealing with prime numbers.
 */
object Primes {

  def logger = LoggerFactory.getLogger("tracer")

  /**
   * Generates a random number of numBits bits with the first and last of these
   * bits set to 1 and all others chosen randomly.
   *
   * @param numBits Number of bits.
   * @return Pseudo-random number of numBits bits with 1's at either end.
   */
  def primeCandidate(numBits: Int): Int = {
    if (numBits < 2)
      throw new IllegalArgumentException

    def randomBit = Random.nextInt & 1

    def loop(numBits: Int, num: Int): Int =
      if (numBits == 0)
        num
      else
        loop(numBits - 1, (num << 1) | randomBit)

    // - seed the loop with 1 which will become the left-most bit
    // - append numBits - 2 random bits
    // - append 1 as the right-most bit
    (loop(numBits - 2, 1) << 1) | 1
  }

  /**
   * Returns the number of bits after all leading zeros of n.
   *
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
   * Tests whether a given number disproves that n is prime.
   *
   * @param a Number used to attempt to disprove that n is prime.
   * @param n Number to check for primality.
   * @return true if a does not disprove n's primality.
   */
  def perhapsPrime(a: Int, n: Int): Boolean = {
    implicit val modVal = ModVal(n)
    implicit def Int2ModMath(n: Int) = ModMath(n)

    logger.trace(s"perhaps prime: n = $n a = $a")

    val x = n - 1

    def loop(i: Int, y: Int): Int =
      if (i == -1)
        y
      else {
        logger.trace(f"x[i=$i]=${(x >> i) & 1}  y=$y%-5d y^2=${y *% y}")
        // found y^2 = 1 for y not +/- 1
        if (y *% y == 1 && y != 1 && y != n - 1)
          y
        else if (((x >> i) & 1) == 1)
          loop(i - 1, y *% y *% a)
        else
          loop(i - 1, y *% y)
      }

    val k = numberOfSignificantBits(x) - 1
    loop(k, 1) == 1
  }
}

package rsas.util

import scala.util.Random
import rsas.util.ModMath.ModVal
import org.slf4j.{Logger, LoggerFactory}

/**
 * A collection of functions dealing with prime numbers.
 */
object Primes {

  private def defaultLogger = LoggerFactory.getLogger("math")

  /**
   * Generates a random number of numBits bits with the first and last of these
   * bits set to 1 and all others chosen randomly.
   *
   * @param numBits Number of bits.
   * @return Pseudo-random number of numBits bits with 1's at either end.
   */
  def primeCandidate(numBits: Int)
                    (implicit logger: Logger = defaultLogger): Int = {
    if (numBits < 2)
      throw new IllegalArgumentException

    def randomBit = {
      val x = Random.nextInt()
      logger.trace(s"[line 97] random=$x bit=${x & 1}")
      x & 1
    }

    def loop(numBits: Int, num: Int): Int =
      if (numBits == 0)
        num
      else
        loop(numBits - 1, (num << 1) | randomBit)

    // - seed the loop with 1 which will become the left-most bit
    // - append numBits - 2 random bits
    // - append 1 as the right-most bit
    val result = (loop(numBits - 2, 1) << 1) | 1
    logger.trace(s"[line 97] result=$result")
    result
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
   * @param logger Logger.
   * @return true if a does not disprove n's primality.
   */
  def perhapsPrime(a: Int, n: Int)
                  (implicit logger: Logger = defaultLogger): Boolean = {
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

  /**
   * Runs the Extended Euclidean algorithm to get the greatest common divisor
   * (GCD) of two numbers A and B as well as s and t such that GCD = sA + tB.
   * @param a A > 0
   * @param b B > 0
   * @return Triple of the form (GCD, s, t).
   * @throws IllegalArgumentException if a or b are <= 0.
   */
  def extendedEuclidGCD(a: Int, b: Int): (Int, Int, Int) = {

    if (a <= 0 || b <= 0)
      throw new IllegalArgumentException

    def loop(i: Int, qi1: Int, qi2: Int, ri: Int, ri1: Int,
             si1: Int, si2: Int, ti1: Int, ti2: Int)
            (implicit logger: Logger = defaultLogger): (Int, Int, Int) = {

      val si = i match {
        case 1 => 1
        case 2 => 0
        case _ => si2 - qi2 * si1
      }

      val ti = i match {
        case 1 => 0
        case 2 => 1
        case _ => ti2 - qi2 * ti1
      }

      if (ri1 == 0)
        (ri, si, ti)
      else {
        val qi = ri / ri1
        val ri2 = ri % ri1

        logger.trace(f"i=$i%-4d  q(i)=$qi%5d  r(i)=${
          ri}%5d  r(i+1)=$ri1%5d  r(i+2)=$ri2%5d  s(i)=$si%5d  t(i)=$ti%5d")

        loop(i = i + 1, qi1 = qi, qi2 = qi1, ri = ri1, ri1 = ri2,
          si1 = si, si2 = si1, ti1 = ti, ti2 = ti1)(logger)
      }
    }

    loop(i = 1, qi1 = 0, qi2 = 0, ri = Math.max(a, b), ri1 = Math.min(a, b),
      si1 = 0, si2 = 0, ti1 = 0, ti2 = 0)
  }
}

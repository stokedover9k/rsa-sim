package rsas

import org.slf4j.{Logger, LoggerFactory}
import rsas.util.Primes

object RSASim {

  val numDisproveAttempts = 20
  val k = 7

  def logger = LoggerFactory.getLogger("math")

  def tracer = LoggerFactory.getLogger("math.trace")

  def candidateStream(length: Int, numBits: Int): Stream[Int] = {
    if (length > 0)
      (Primes.primeCandidate(numBits + 1) & ~(1 << numBits)) #::
        candidateStream(length - 1, numBits)
    else if (length == 0)
      Stream()
    else
      throw new IllegalArgumentException
  }

  /**
   * Tries to disprove that a number is prime using the provided numbers.
   *
   * @param prime Number to test.
   * @param candidates Numbers to use for disproving primality.
   * @return Some(num) if num disproved primality, None otherwise.
   */
  def disprovePrime(prime: Int, candidates: Iterable[Int]): Option[Int] = {
    if (candidates.isEmpty)
      None
    else if (Primes.perhapsPrime(candidates.head, prime))
      disprovePrime(prime, candidates.tail)
    else
      Some(candidates.head)
  }

  def findNotPrime: Int = {
    val prime = Primes.primeCandidate(k)

    disprovePrime(prime, candidateStream(numDisproveAttempts, k - 1)) match {
      case None => findNotPrime
      case Some(a) => {
        tracer.trace(s"[line 112] primality of $prime disproved by $a")
        Primes.perhapsPrime(a, prime)(tracer)
        prime
      }
    }
  }

  def findPrime(tracer: Logger): Int = {
    val prime = Primes.primeCandidate(k)

    disprovePrime(prime, candidateStream(numDisproveAttempts, k - 1)) match {
      case Some(_) => findPrime(tracer)
      case None => {
        val extraCandidate = candidateStream(1, k - 1)
        disprovePrime(prime, extraCandidate) match {
          case Some(_) => findPrime(tracer)
          case None => {
            val a = extraCandidate.head
            tracer.trace(s"[line 115] primality of $prime supported by e.g. $a")
            Primes.perhapsPrime(a, prime)(tracer)
            prime
          }
        }
      }
    }
  }

  def findPrimeDifferentThan(p: Int)(logger: Logger): Int = {
    val q = findPrime(logger)
    if (q == p)
      findPrimeDifferentThan(p)(logger)
    else
      q
  }

  def main(args: Array[String]): Unit = {
    // Trace generation of a random potential prime
    Primes.primeCandidate(k)(tracer)

    // Find a non-prime and trace the disprove of its primality
    findNotPrime

    // Find a probably-prime and trace a support of its primality
    val p = findPrime(tracer)

    // Find another prime different from p. Do not show trace this time.
    val q = findPrimeDifferentThan(p)(logger)

    val n = p * q
    val phiN = (p - 1) * (q - 1)


  }
}
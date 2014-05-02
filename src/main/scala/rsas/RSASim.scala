package rsas

import org.slf4j.{Logger, LoggerFactory}
import rsas.util.{ModMath, Primes}
import rsas.util.ModMath.ModVal

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

  def findPublicPrivateKeyPair(phiN: Int): (Int, Int) = {

    def loop(e: Int): Option[(Int, Int)] = {
      tracer.trace(s"[line 133] proposed public key e=$e")
      if (e == phiN)
        None
      else
        Primes.extendedEuclidGCD(phiN, e)(tracer) match {
          case (1, s, t) => {
            tracer.trace(s"[line 133] public key e=$e accepted")
            val d = ModMath(t)(ModVal(phiN)).num
            Some(e, d)
          }
          case _ => {
            tracer.trace(s"[line 133] public key e=$e rejected")
            loop(e + 1)
          }
        }
    }

    loop(3) match {
      case None => throw new IllegalStateException(
        s"could not find a key pair for phi=$phiN")
      case Some((e, d)) => {
        tracer.trace(s"[line 143] private key d=$d")
        (e, d)
      }
    }
  }

  def generateKeyPair(implicit tracer: Logger): KeyPair = {
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

    tracer.trace(s"n=$n phi(n)=$phiN")

    val (e, d) = findPublicPrivateKeyPair(phiN)

    tracer.trace(s"[line 147] p=(${int2BitString(p)})=$p")
    tracer.trace(s"[line 147] q=(${int2BitString(q)})=$q")
    tracer.trace(s"[line 147] n=(${int2BitString(n)})=$n")
    tracer.trace(s"[line 147] e=(${int2BitString(e)})=$e")
    tracer.trace(s"[line 147] d=(${int2BitString(d)})=$d")

    KeyPair(n = n, e = e, d = d)
  }

  def main(args: Array[String]): Unit = {

    // only tracing for Alice
    val alice = KeyHolder("Alice", generateKeyPair(tracer))
    val trent = KeyHolder("Trent", generateKeyPair(logger))

    val certificateAlice = trent.getSigned(alice.certificate)

    tracer.trace(s"[line 175] Alice's certificate:        ${bytes2BitString(certificateAlice.data.bytes)}")
    tracer.trace(s"[line 175] Alice's certificate hash:   ${bytes2BitString(trent.hash(certificateAlice.data.bytes))}")
    tracer.trace(s"[line 175] Trent's signature on Alice: ${bytes2BitString(certificateAlice.signature)}")

    tracer.trace(s"[line 177] Alice's certificate hash:   ${bytes2Int(trent.hash(certificateAlice.data.bytes))}")
    tracer.trace(s"[line 177] Trent's signature on Alice: ${bytes2Int(certificateAlice.signature)}")

    // Since prime candidate sets the 1st and the kth (from the right) bits to
    // 1, we can use it to create Bob's u if we discard the 1st (least
    // significant) bit.
    // Called k1 because k1 = k + 1 where k is as described in instructions,
    // the index (0-indexed) of the last 1-bit (from right/least significant).
    val k1 = numberOfSignificantBits(alice.n)
    val u = Primes.primeCandidate(k1) >> 1

    tracer.trace(s"[line 195] k=${k1 - 1} u=$u")
    tracer.trace(s"[line 197] u = ${int2BitString(u)}")

    // these bytes are sent to alice to sign
    val uData = int2Bytes(u)

    // these bytes are returned by alice to bob
    val vData = alice.auth.getSigned(uData)


  }
}

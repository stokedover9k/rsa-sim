package rsas.util

import org.specs2.mutable.Specification

class PrimesSpec extends Specification {

  "Primes.candidatePrime" should {

    "generate values with first and last bits set to 1" in {

      def check(num: Int, numBits: Int) = {
        num must be lessThan 1 << numBits
        num must be greaterThan 1 << (numBits - 1)
      }

      for (i <- 2 to 10)
        check(Primes.primeCandidate(i), i)

      check(Primes.primeCandidate(16), 16)
    }

    "throw if number of bits < 2" in {
      Primes.primeCandidate(1) must throwA[IllegalArgumentException]
      Primes.primeCandidate(0) must throwA[IllegalArgumentException]
      Primes.primeCandidate(-2) must throwA[IllegalArgumentException]
    }

    "work for minimal length (2 bits returns 3)" in {
      Primes.primeCandidate(2) must be equalTo 3
    }
  }

  "Primes.perhapsPrime" should {

    "be correct" in {
      Primes.perhapsPrime(7, 21) must beFalse
      Primes.perhapsPrime(5, 17) must beTrue
      Primes.perhapsPrime(2, 27) must beFalse
      Primes.perhapsPrime(2, 61) must beTrue
      Primes.perhapsPrime(5, 8) must beFalse
    }
  }

  "Primes.numberOfSignificantBits" should {

    "compute correct for x = 0" in {
      Primes.numberOfSignificantBits(0) mustEqual 0
    }

    "compute correct for x = 1" in {
      Primes.numberOfSignificantBits(1) mustEqual 1
    }

    "compute correct for x = 16" in {
      Primes.numberOfSignificantBits(0x10) mustEqual 5
    }

    "compute correct for some other values" in {
      Primes.numberOfSignificantBits(0x17) mustEqual 5
    }
  }
}

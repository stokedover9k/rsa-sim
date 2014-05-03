package rsas.util

import org.specs2.mutable.Specification
import rsas.numberOfSignificantBits

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

  "numberOfSignificantBits" should {

    "compute correct for x = 0" in {
      numberOfSignificantBits(0) mustEqual 0
    }

    "compute correct for x = 1" in {
      numberOfSignificantBits(1) mustEqual 1
    }

    "compute correct for x = 16" in {
      numberOfSignificantBits(0x10) mustEqual 5
    }

    "compute correct for some other values" in {
      numberOfSignificantBits(0x17) mustEqual 5
    }
  }

  "Primes.extendedEuclideanGCD" should {

    "compute correct gcd" in {
      Primes.extendedEuclidGCD(75, 28) mustEqual(1, 3, -8)
    }

    "compute correctly when A and B are reversed" in {
      Primes.extendedEuclidGCD(75, 28) mustEqual Primes.
        extendedEuclidGCD(28, 75)
    }

    "work when A and B are not relatively prime" in {
      Primes.extendedEuclidGCD(10, 8) match {
        case (gcd, s, t) => gcd mustEqual 2
      }
    }
  }
}

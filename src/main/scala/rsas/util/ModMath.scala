package rsas.util

/**
 * A wrapper for integers to allow operations with a modulus.
 *
 * @param num Numeric value of this object.
 */
class ModMath private(val num: Int) {

  import rsas.util.ModMath.ModVal

  /**
   * Addition modulo n: this +_n rightTerm, where +_n is addition modulo n.
   *
   * @param rightTerm Right factor of addition.
   * @param n Modulus.
   * @return This +_n rightTerm.
   */
  def +%(rightTerm: Int)(implicit n: ModVal): Int =
    ModMath.mod(num + rightTerm, n.mod)

  /**
   * Subtraction modulo n: this -_n subtrahend, where -_n is subtraction modulo
   * n.
   *
   * @param subtrahend Value to subtract.
   * @param n Modulus.
   * @return This -_n subtrahend.
   */
  def -%(subtrahend: Int)(implicit n: ModVal): Int =
    ModMath.mod(num - subtrahend, n.mod)

  /**
   * Multiplication modulo n: this *_n rightFactor, where *_n is multiplication
   * modulo n.
   *
   * @param rightFactor Right term of multiplication.
   * @param n Modulus.
   * @return This *_n rightFactor.
   */
  def *%(rightFactor: Int)(implicit n: ModVal): Int =
    ModMath.mod(num * rightFactor, n.mod)

  /**
   * Fast exponentiation modulo n: this to the positive power p modulo n.
   * @param p Power to raise this to.
   * @param n Modulus.
   * @return This to power p.
   * @throws IllegalArgumentException if p < 0
   */
  def **%(p: Int)(implicit n: ModVal): Int = {
    if (p < 0)
      throw new IllegalArgumentException

    implicit def Int2ModMath(i: Int): ModMath = ModMath(i)

    def loop(i: Int, y: Int): Int = i match {
      case -1 => y
      case _ =>
        if ((p & (1 << i)) == 0)
          loop(i - 1, y *% y)
        else
          loop(i - 1, y *% y *% num)
    }

    loop(Primes.numberOfSignificantBits(p) - 1, 1)
  }
}

/**
 * A wrapper for integers to allow operations with a modulus.
 */
object ModMath {

  /**
   * Modulus value.
   *
   * @param mod Modulus > 0.
   */
  case class ModVal(mod: Int) {
    validateMod(mod)
  }

  /**
   * Constructs a new ModMath value scaling it into the range of the modulus.
   *
   * @param num Value to lift to ModMath.
   * @param n Modulus.
   * @return New ModMath wrapper.
   */
  def apply(num: Int)(implicit n: ModVal): ModMath =
    new ModMath(mod(num, n.mod))

  /**
   * Returns num modulo mod.
   *
   * @param num Number to reduce.
   * @param mod Modulus.
   * @return Number modulo mod.
   */
  def mod(num: Int, mod: Int): Int = {
    validateMod(mod)
    if (num < 0)
      num + mod * (-num / mod + 1)
    else
      num - mod * (num / mod)
  }

  private def validateMod(num: Int) {
    if (num <= 0)
      throw new IllegalArgumentException("mod must be positive")
  }
}

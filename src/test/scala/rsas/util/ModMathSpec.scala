package rsas.util

import org.specs2.mutable._
import org.specs2.specification.Scope

import ModMath.ModVal

class ModMathSpec extends Specification {

  "ModVal" should {
    "throw when negative" in {
      ModVal(-1) must throwA[IllegalArgumentException]
    }

    "throw when zero" in {
      ModVal(0) must throwA[IllegalArgumentException]
    }
  }

  trait MathMod5 extends Scope {
    implicit def Int2ModMath(num: Int)(implicit mod: ModVal) =
      ModMath(num)(mod)

    implicit val mod = ModVal(5)
  }

  "ModMath" should {

    "construct correctly with values >= 0" in new MathMod5 {
      ModMath(4).num must_== 4
      ModMath(0).num must_== 0
    }

    "construct correctly with values < 0" in new MathMod5 {
      ModMath(-1).num must_== 4
      ModMath(-6).num must_== 4
    }

    "construct correctly with values >= mod" in new MathMod5 {
      ModMath(5).num must_== 0
      ModMath(6).num must_== 1
      ModMath(11).num must_== 1
    }
  }

  "ModMath.mod" should {

    "throw if mod <= 0" in {
      ModMath.mod(5, -2) must throwA[IllegalArgumentException]
      ModMath.mod(5, 0) must throwA[IllegalArgumentException]
    }

    "correct when x is in range" in {
      ModMath.mod(2, 5) mustEqual 2
    }

    "correct when x = 0" in {
      ModMath.mod(0, 5) mustEqual 0
    }

    "correct when x < 0" in {
      ModMath.mod(-1, 5) mustEqual 4
      ModMath.mod(-6, 5) mustEqual 4
    }

    "correct when x >= mod" in {
      ModMath.mod(5, 5) mustEqual 0
      ModMath.mod(6, 5) mustEqual 1
      ModMath.mod(11, 5) mustEqual 1
    }
  }

  "ModMath" should {
    "correctly do addition mod n" in new MathMod5 {
      2 +% 2 mustEqual 4
      2 +% 3 mustEqual 0
      2 +% 4 mustEqual 1
      (-3) +% 4 mustEqual 1
      2 +% (-3) mustEqual 4
    }

    "correctly do subtraction mod n" in new MathMod5 {
      2 -% 2 mustEqual 0
      2 -% 3 mustEqual 4
      3 -% 2 mustEqual 1
      (-3) -% 3 mustEqual 4
      3 -% (-3) mustEqual 1
    }

    "correctly do multiplication mod n" in new MathMod5 {
      2 *% 2 mustEqual 4
      3 *% 3 mustEqual 4
      2 *% (-3) mustEqual 4
    }
  }
}

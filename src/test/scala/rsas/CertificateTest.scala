package rsas

import org.specs2.mutable._
import org.specs2.specification.Scope

class CertificateTest extends Specification {

  trait AliceCert extends Scope {

    val name = "Alice"

    // note that 80 will result in a "negative" byte value
    val modulus = (35 << 8) + 80

    val exponent = 13

    lazy val cert = Certificate(name, modulus, exponent)
  }

  "Certificate" should {
    "return the right values through getters" in new AliceCert {
      Certificate.Name.valueOf(name) mustEqual cert.getName
      modulus mustEqual cert.getN
      exponent mustEqual cert.getE
    }

    "have the certificate of the right length" in new AliceCert {
      Certificate.length mustEqual cert.bytes.length
    }
  }
}

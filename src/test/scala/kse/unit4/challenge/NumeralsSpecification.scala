package kse.unit4.challenge

import kse.unit4.challenge.generators.given
import kse.unit4.challenge.numerals.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import org.scalacheck.Test.Parameters

object NumeralsSpecification extends Properties("Numerals"):

  override def overrideParameters(p: Parameters): Parameters =
    p.withMinSuccessfulTests(50).withMaxDiscardRatio(100)

  property("Zero.isZero and toInt") = forAll { (z: Zero) =>
    z.isZero && z.toInt == 0
  }

  property("Zero equality") = forAll { (z: Zero) =>
    z == Zero
  }

  property("Successor is not zero") = forAll { (n: Numeral) =>
    val s = n.successor
    !s.isZero && s.toInt == n.toInt + 1
  }

  property("Predecessor of successor is original numeral") = forAll { (n: Numeral) =>
    if n.isZero then true else n.successor.predecessor == n
  }

  property("Addition with Zero") = forAll { (n: Numeral) =>
    (n + Zero) == n && (Zero + n) == n
  }

  property("Addition consistency") = forAll { (n: Numeral, m: Numeral) =>
    (n + m).toInt == n.toInt + m.toInt
  }

  property("Subtraction: Zero - Zero is Zero") = forAll { (z: Zero) =>
    (z - z) == Zero
  }

  property("Subtraction: Zero - nonZero throws exception") = forAll { (n: Numeral) =>
    n.isZero || throws(classOf[NoSuchElementException]) { Zero - n }
  }

  property("Subtraction: negative result throws exception") = forAll { (n: Numeral, m: Numeral) =>
    if n.toInt < m.toInt then throws(classOf[NoSuchElementException]) { n - m } else true
  }

  property("Subtraction consistency (non-negative)") = forAll { (n: Numeral, m: Numeral) =>
    if n.toInt >= m.toInt then (n - m).toInt == n.toInt - m.toInt else true
  }

  property("Ordering: greater than") = forAll { (n: Numeral, m: Numeral) =>
    (n > m) == (n.toInt > m.toInt)
  }

  property("Ordering: less than") = forAll { (n: Numeral, m: Numeral) =>
    (n < m) == (n.toInt < m.toInt)
  }

  property("Ordering: greater or equal") = forAll { (n: Numeral, m: Numeral) =>
    (n >= m) == (n.toInt >= m.toInt)
  }

  property("Ordering: less or equal") = forAll { (n: Numeral, m: Numeral) =>
    (n <= m) == (n.toInt <= m.toInt)
  }

  property("Equality is reflexive") = forAll { (n: Numeral) =>
    n == n
  }

  property("Equality is symmetric") = forAll { (n: Numeral, m: Numeral) =>
    (n == m) == (m == n)
  }

end NumeralsSpecification

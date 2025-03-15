package kse.unit5.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object set:

  trait NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean

    infix def exists(predicate: Numeral => Boolean): Boolean

    infix def contains(x: Numeral): Boolean

    infix def include(x: Numeral): NumeralSet

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet

  // Optional
  // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet

  // Optional
  // Uncomment if needed
//    @targetName("symmetric difference")
//    infix def ∆(that: NumeralSet): NumeralSet = ???

  end NumeralSet

  type Empty = Empty.type

  case object Empty extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean = true

    infix def exists(predicate: Numeral => Boolean): Boolean = false

    infix def contains(x: Numeral): Boolean = false

    infix def include(x: Numeral): NumeralSet = NonEmpty(Empty, x, Empty)

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet = ???

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet = that

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet = Empty

    // Optional
    // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet = ???

    override def toString: String = "[*]"

    override def equals(obj: Any): Boolean =
      obj match
        case that: NumeralSet => that.forAll(_ => false)
        case _                => false

  end Empty

  case class NonEmpty(left: NumeralSet, element: Numeral, right: NumeralSet) extends NumeralSet:

    infix def forAll(predicate: Numeral => Boolean): Boolean =
      predicate(element) && (left.forAll(predicate)) && (right.forAll(predicate))

    infix def exists(predicate: Numeral => Boolean): Boolean =
      predicate(element) || (left.exists(predicate)) || (right.exists(predicate))

    infix def contains(x: Numeral): Boolean =
      if x == element then true
      else if x < element then left.contains(x)
      else right.contains(x)

    infix def include(x: Numeral): NumeralSet =
      if x == element then this
      else if x < element then NonEmpty(left.include(x), element, right)
      else NonEmpty(left, element, right.include(x))

    // Optional
    // Uncomment if needed
//    infix def remove(x: Numeral): NumeralSet = ???

    @targetName("union")
    infix def ∪(that: NumeralSet): NumeralSet =
      (left ∪ (right ∪ that)).include(element)

    @targetName("intersection")
    infix def ∩(that: NumeralSet): NumeralSet =
      val leftIntersection  = left ∩ that
      val rightIntersection = right ∩ that

      if that.contains(element) then (leftIntersection ∪ rightIntersection).include(element)
      else leftIntersection ∪ rightIntersection

    // Optional
    // Uncomment if needed
//    @targetName("difference")
//    infix def \(that: NumeralSet): NumeralSet = ???

    override def toString: String = s"[$left - [$element] - $right]"

    override def equals(obj: Any): Boolean =
      obj match
        case that: NumeralSet => this.forAll(e => that.contains(e)) && that.forAll(e => this.contains(e))
        case _                => false

  end NonEmpty

end set

package kse.unit4.challenge

import scala.annotation.targetName

object numerals:

  trait Numeral:

    def isZero: Boolean

    def predecessor: Numeral

    def successor: Numeral = new Successor(this)

    @targetName("greater than")
    infix def >(that: Numeral): Boolean

    @targetName("greater or equal to")
    infix def >=(that: Numeral): Boolean = (this > that) || (this == that)

    @targetName("less than")
    infix def <(that: Numeral): Boolean = !(this >= that)

    @targetName("less or equal to")
    infix def <=(that: Numeral): Boolean = !(this > that)

    @targetName("addition")
    infix def +(that: Numeral): Numeral

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral

    def toInt: Int

    override def toString: String = s"Nat($predecessor)"

  type Zero = Zero.type

  object Zero extends Numeral:

    def isZero: Boolean = true

    def predecessor: Numeral =
      throw new NoSuchElementException("Zero has no predecessor")

    @targetName("greater than")
    infix def >(that: Numeral): Boolean = false

    @targetName("addition")
    infix def +(that: Numeral): Numeral = that

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      if that.isZero then Zero
      else throw new NoSuchElementException("Negative result in natural number subtraction")

    def toInt: Int = 0

    override def toString: String = "Nat(0)"

    override def equals(obj: Any): Boolean =
      obj match
        case Zero => true
        case _    => false

  object Successor:
    def unapply(successor: Successor): Option[Numeral] = Option(successor.predecessor)

  class Successor(n: Numeral) extends Numeral:

    def isZero: Boolean = false

    def predecessor: Numeral = n

    @targetName("greater than")
    infix def >(that: Numeral): Boolean =
      that match
        case Zero    => true
        case another => n > another.predecessor

    @targetName("addition")
    infix def +(that: Numeral): Numeral = new Successor(n + that)

    // Optional
    @targetName("subtraction")
    infix def -(that: Numeral): Numeral =
      that match
        case Zero         => this
        case s: Successor => n - s.predecessor

    def toInt: Int = 1 + n.toInt

    override def toString: String = s"Nat($toInt)"

    override def equals(obj: Any): Boolean = obj match
      case another: Successor => this.predecessor == another.predecessor
      case _                  => false

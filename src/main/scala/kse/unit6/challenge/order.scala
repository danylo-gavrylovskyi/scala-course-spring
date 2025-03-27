package kse.unit6.challenge

import kse.unit4.challenge.numerals.Numeral
import scala.annotation.targetName

object order:

  trait Order[T]:
    def compare(left: T, right: T): Int

  object Order:

    given Order[Numeral] with
      def compare(x: Numeral, y: Numeral): Int = x.toInt - y.toInt

  extension [V: Order as ord](elem: V)

    infix def >(that: V): Boolean =
      ord.compare(elem, that) > 0

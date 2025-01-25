package kse.unit1.challenge

import scala.annotation.tailrec

object arithmetic:

  type Number = Long

  val increment: Number => Number =
    value => value + 1

  val decrement: Number => Number =
    value => value - 1

  val isZero: Number => Boolean =
    value => value == 0

  val isNonNegative: Number => Boolean =
    value => value >= 0

  val abs: Number => Number =
    value =>
      if isNonNegative(value) then value
      else -value

  @tailrec
  def addition(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    if isZero(right) then left
    else addition(increment(left), decrement(right))

  @tailrec
  def multiplication(left: Number, right: Number, accumulator: Number = 0): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    if isZero(right) then accumulator
    else multiplication(left, decrement(right), addition(accumulator, left))

  @tailrec
  def power(base: Number, p: Number, accumulator: Number = 1): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    if isZero(p) then accumulator
    else power(base, decrement(p), multiplication(accumulator, base))

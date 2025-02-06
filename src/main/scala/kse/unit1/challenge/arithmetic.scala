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

  def multiplication(left: Number, right: Number): Number =
    require(left >= 0, "Left must be non-negative")
    require(right >= 0, "Right must be non-negative")

    @tailrec
    def multiplicationReq(left: Number, right: Number, accumulator: Number): Number =
      if isZero(right) then accumulator
      else multiplicationReq(left, decrement(right), addition(accumulator, left))

    multiplicationReq(left, right, 0)

  def power(base: Number, p: Number): Number =
    require(p >= 0, "Power must be non-negative")
    require(base != 0 || p != 0, "0^0 is undefined")

    @tailrec
    def powerReq(base: Number, p: Number, accumulator: Number): Number =
      if isZero(p) then accumulator
      else powerReq(base, decrement(p), multiplication(accumulator, base))

    powerReq(base, p, 1)

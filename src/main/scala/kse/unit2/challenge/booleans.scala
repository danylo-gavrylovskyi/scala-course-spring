package kse.unit2.challenge

import scala.annotation.{tailrec, targetName}

object booleans:

  case object True
  case object False

  type True    = True.type
  type False   = False.type
  type Boolean = True | False

  val negation: Boolean => Boolean =
    case True  => False
    case False => True

  val conjunction: (Boolean, => Boolean) => Boolean =
    (left, right) =>
      left match
        case False => False
        case True  => right

  val disjunction: (Boolean, => Boolean) => Boolean =
    (left, right) =>
      left match
        case False => right
        case True  => True

  val implication: (Boolean, => Boolean) => Boolean =
    (left, right) => (!left) ∨ right

  val equivalence: (Boolean, => Boolean) => Boolean =
    (left, right) => (left → right) ∧ (right → left)

  extension (value: Boolean)

    @targetName("negation")
    infix def unary_! : Boolean = negation(value)

    @targetName("conjunction")
    infix def ∧(that: => Boolean): Boolean = conjunction(value, that)

    @targetName("disjunction")
    infix def ∨(that: => Boolean): Boolean = disjunction(value, that)

    @targetName("implication")
    infix def →(that: => Boolean): Boolean = implication(value, that)

    @targetName("equivalence")
    infix def ↔(that: => Boolean): Boolean = equivalence(value, that)

  def fold(operation: (Boolean, Boolean) => Boolean, unit: Boolean)(list: List[Boolean]): Boolean =
    list match
      case head :: tail => operation(head, fold(operation, unit)(tail))
      case Nil          => unit

  val conjunctionOfElements: List[Boolean] => Boolean = fold(_ ∧ _, True)
  val disjunctionOfElements: List[Boolean] => Boolean = fold(_ ∨ _, False)

  extension (booleans: List[Boolean])
    infix def conjunction: Boolean = conjunctionOfElements(booleans)
    infix def disjunction: Boolean = disjunctionOfElements(booleans)

import cats.implicits._
import cats.parse.Parser

import Helper.parsePrint

object Example1 {
  sealed abstract class Day extends Product with Serializable
  final case object Monday extends Day
  final case object Tuesday extends Day
  final case object Wednesday extends Day
  final case object Thursday extends Day
  final case object Friday extends Day
  final case object Saturday extends Day
  final case object Sunday extends Day

  final case class DayRange(start: Day, end: Day)

  /**
    * The `oneOf` combinator tries parsing, one parser after the next
    * @example parsePrint(simpleDayP, "Mo")
    */
  val simpleDayP: Parser[Unit] =
    Parser.oneOf(List(
      Parser.string("Mo"),
      Parser.string("Tu"),
      Parser.string("We"),
      Parser.string("Th"),
      Parser.string("Fr"),
      Parser.string("Sa"),
      Parser.string("Su")
    ))

  /**
    * The `as` function will return a constant value if the parser succeeds
    * `.map(_ => x)` is the same as `.as(x)`
    *
    * @example parsePrint(dayP, "Mo")
    * @example parsePrint(dayP, "hello")
    */
  val dayP: Parser[Day] =
    Parser.oneOf(List(
      Parser.string("Mo").map(_ => Monday),
      Parser.string("Tu").as(Tuesday),
      Parser.string("We").as(Wednesday),
      Parser.string("Th").as(Thursday),
      Parser.string("Fr").as(Friday),
      Parser.string("Sa").as(Saturday),
      Parser.string("Su").as(Sunday)
    ))

  /**
    * We can use for comprehensions to combine parsers
    * This is because Parser is a Monad!
    * @example parsePrint(simpleDayRangeP, "MoTu")
    * @example parsePrint(simpleDayRangeP, "Mo-Tu")
    */
  val simpleDayRangeP: Parser[DayRange] = for {
    start <- dayP
    end   <- dayP
  } yield DayRange(start, end)

  /**
    * Parse two days, with a hyphen between
    * @example parsePrint(dayRangeP, "Mo-Tu")
    */
  val dayRangeP: Parser[DayRange] = for {
    start <- dayP <* Parser.char('-')
    end   <- dayP
  } yield DayRange(start, end)

  /**
    * mapN is another way to combine multiple parsers
    * For parsers it's faster than using "for". Prefer mapN if possible
    * @example parsePrint(dayRange2P, "Mo-Tu")
    */
  val dayRange2P: Parser[DayRange] =
    (
      dayP <* Parser.char('-'),
      dayP
    ).mapN(DayRange)
}

import Example1._

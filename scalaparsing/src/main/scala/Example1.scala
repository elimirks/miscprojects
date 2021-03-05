package scalaparsing

import scala.util.Try
import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}

/**
  * scalaparsing.Example1.simpleDayP.parseAll("Mo")
  * scalaparsing.Example1.dayP.parseAll("Mo")
  * scalaparsing.Example1.dayP.parseAll("hello")
  * scalaparsing.Example1.simpleDayRangeP.parseAll("MoTu")
  * scalaparsing.Example1.simpleDayRangeP.parseAll("Mo-Tu")
  * scalaparsing.Example1.dayRangeP.parseAll("Mo-Tu")
  */
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
    * The `as` combinator will return a static value if the parser succeeds
    */
  val dayP: Parser[Day] =
    Parser.oneOf(List(
      Parser.string("Mo").as(Monday),
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
    */
  val simpleDayRangeP: Parser[DayRange] = for {
    start <- dayP
    end   <- dayP
  } yield DayRange(start, end)

  /**
    * Parse two days, with a hyphen between
    */
  val dayRangeP: Parser[DayRange] = for {
    start <- dayP <* Parser.char('-')
    end   <- dayP
  } yield DayRange(start, end)
}

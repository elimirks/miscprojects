package scatto

import scala.util.Try
import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}

/**
  * Simplified OSM format parser.
  */
object CatsOSMParser {
  val whitespace0: Parser0[Unit] =
    Parser.charIn(" \t\r\n").rep0.void

  val intPadZeroP: Parser[Int] =
    Numbers.digits.flatMap(digits => {
      Try(digits.toInt).toOption match {
        case Some(value) => Parser.pure(value)
        case _           => Parser.Fail
      }
    })

  def mkDayParser(abbreviation: String, day: Day): Parser[Day] =
    Parser.string(abbreviation)
      .map(_ => day)
      .backtrack

  val dayP: Parser[Day] =
    mkDayParser("Mo", Monday)    <+>
    mkDayParser("Tu", Tuesday)   <+>
    mkDayParser("We", Wednesday) <+>
    mkDayParser("Th", Thursday)  <+>
    mkDayParser("Fr", Friday)    <+>
    mkDayParser("Sa", Saturday)  <+>
    mkDayParser("Su", Sunday)

  val dayRangeP: Parser[DayRange] = for {
    start <- dayP
    _     <- Parser.char('-')
    end   <- dayP
  } yield DayRange(start, end)

  val hourP: Parser[Int] =
    intPadZeroP.filter(x => x >= 0 && x <= 23)

  val minuteP: Parser[Int] =
    intPadZeroP.filter(x => x >= 0 && x <= 59)

  val timeP: Parser[Time] = for {
    hour   <- hourP
    _      <- Parser.char(':')
    minute <- minuteP
  } yield Time(hour, minute)

  val timeRangeP: Parser[TimeRange] = for {
    start <- timeP
    _     <- Parser.char('-')
    end   <- timeP
  } yield TimeRange(start, end)

  val recordP: Parser[Record] = for {
    days       <- dayRangeP
    _          <- whitespace0
    timeRanges <- timeRangeP.repSep0(Parser.char(','))
  } yield Record(days, timeRanges)

  val recordsP: Parser0[Seq[Record]] =
    whitespace0 *> recordP.repSep0(Parser.char(';')) <* whitespace0

  def parse(input: String): Option[Seq[Record]] = {
    recordsP.parseAll(input).toOption
  }
}

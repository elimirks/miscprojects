package scatto

import atto._
import Atto._
import cats.implicits._

/**
  * Simplified OSM format parser.
  */
object AttoOSMParser {
  def mkDayParser(abbreviation: String, day: Day): Parser[Day] =
    string(abbreviation).map(_ => day)

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
    _     <- char('-')
    end   <- dayP
  } yield DayRange(start, end)

  val hourP: Parser[Int] =
    int.filter(value => value >= 0 && value <= 23)

  val minuteP: Parser[Int] =
    int.filter(value => value >= 0 && value <= 59)

  val timeP: Parser[Time] = for {
    hour   <- hourP
    _      <- char(':')
    minute <- minuteP
  } yield Time(hour, minute)

  val timeRangeP: Parser[TimeRange] = for {
    start <- timeP
    _     <- char('-')
    end   <- timeP
  } yield TimeRange(start, end)

  val recordP: Parser[Record] = for {
    days       <- dayRangeP
    _          <- many(spaceChar)
    timeRanges <- timeRangeP.sepBy(char(','))
  } yield Record(days, timeRanges)

  val recordsP: Parser[Seq[Record]] =
    recordP.sepBy(char(';'))

  def parse(input: String): Option[Seq[Record]] = {
    recordsP.parseOnly(input).option
  }
}

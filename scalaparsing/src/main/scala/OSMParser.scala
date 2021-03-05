import scala.util.Try
import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}
import cats.data.NonEmptyList

/**
  * Simplified OSM format parser.
  */
object OSMParser {
  /**
    * Parser to consume and ignore all types of whitespace
    */
  val whitespace0P: Parser0[Unit] =
    Parser.charIn(" \t\r\n").rep0.void

  /**
    * Parses an int with optionally left-padded zeros 
    * For example "00004" will parse into the into 4
    */
  val intPadZeroP: Parser[Int] =
    Numbers.digits.flatMap(digits => {
      Try(digits.toInt).toOption match {
        case Some(value) => Parser.pure(value)
        case _           => Parser.Fail
      }
    })

  /**
    * Makes a parser for single character delimiters
    * Ignore surrounding whitespace, and discards the character itself
    * For example, delimiterP('-') will parse "     - \t"
    *
    * @param c The delimiter character to parse
    */
  def delimiterP(c: Char): Parser[Unit] =
    Parser.char(c)
      .surroundedBy(whitespace0P)
      .void

  /**
    * Parses an OSM day strings
    * For example "Mo" turns into Monday
    *             "Sa" turns into Saturday
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
    * Parses a day range, delimited with '-'
    * For example, Mo-We turns into DayRange(Monday, Wednesday)
    */
  val dayRangeP: Parser[DayRange] = for {
    start <- dayP <* delimiterP('-')
    end   <- dayP
  } yield DayRange(start, end)

  /**
    * Parses an hour from 0-23
    */
  val hourP: Parser[Int] =
    intPadZeroP.filter(x => x >= 0 && x <= 23)

  /**
    * Parses a minute from 0-59
    */
  val minuteP: Parser[Int] =
    intPadZeroP.filter(x => x >= 0 && x <= 59)

  /**
    * Parses a timestamp
    * For example "10:30" turns into Time(10, 30)
    */
  val timeP: Parser[Time] = for {
    hour   <- hourP <* delimiterP(':')
    minute <- minuteP
  } yield Time(hour, minute)

  /**
    * Parses a time range
    * For example "10:30 - 11:30" turns into:
    * TimeRange(Time(10, 30), Time(11, 30))
    */
  val timeRangeP: Parser[TimeRange] = for {
    start <- timeP <* delimiterP('-')
    end   <- timeP
  } yield TimeRange(start, end)

  /**
    * Parses multiple time ranges
    * For example "10:30 - 11:30, 12:30-14:00"
    */
  val timeRangesP: Parser[NonEmptyList[TimeRange]] =
    timeRangeP.repSep(delimiterP(','))

  /**
    * Parses a single OSM opening hour record
    * For example "Mo-We 10:30 - 11:30, 12:45"
    */
  val recordP: Parser[Record] = for {
    days       <- dayRangeP <* whitespace0P
    timeRanges <- timeRangesP
  } yield Record(days, timeRanges)

  /**
    * Parses mulitple OSM open hour records, delimeted by ';'
    */
  val recordsP: Parser[NonEmptyList[Record]] =
    recordP
      .repSep(delimiterP(';'))
      .surroundedBy(whitespace0P)

  def parse(input: String): Option[NonEmptyList[Record]] = {
    recordsP.parseAll(input).toOption
  }
}

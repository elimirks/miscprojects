import scala.util.Try
import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}
import cats.data.NonEmptyList

import Helper.parsePrint

/**
  * Simplified OSM format parser.
  */
object OSMParser {
  /**
    * Parser to consume and ignore any amount of horizontal whitespace
    */
  val whitespace0P: Parser0[Unit] =
    Parser.charIn(" \t").rep0.void

  /**
    * `.toInt` will throw an exception if the int is out of bounds
    * This function instead returns `None` on failure
    */
  def safeToInt(value: String): Option[Int] =
    Try(value.toInt).toOption

  /**
    * Parses an unsigned int with optionally left-padded zeros 
    * Fails for values outside the bounds of Int
    *
    * @example parsePrint(uintP, "2343")
    * @example parsePrint(uintP, "0004")
    */
  val uintP: Parser[Int] =
    Numbers.digits.mapFilter(safeToInt)

  /**
    * Makes a parser for single character delimiters
    * Ignore surrounding whitespace, and discards the character itself
    *
    * @example parsePrint(delimiterP('-'), "     - \t")
    * @param c The delimiter character to parse
    */
  def delimiterP(c: Char): Parser[Unit] =
    Parser.char(c)
      .surroundedBy(whitespace0P)
      .void

  /**
    * Parses an OSM day strings
    * @example parsePrint(dayP, "Mo")
    * @example parsePrint(dayP, "Sa")
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
    * @example parsePrint(dayRangeP, "Mo-We")
    */
  val dayRangeP: Parser[DayRange] = (
    dayP <* delimiterP('-'),
    dayP
  ).mapN(DayRange)

  /**
    * Parses an hour from 0-23
    * @example parsePrint(hourP, "3")
    */
  val hourP: Parser[Int] =
    uintP.filter(x => x >= 0 && x <= 23)

  /**
    * Parses a minute from 0-59
    */
  val minuteP: Parser[Int] =
    uintP.filter(x => x >= 0 && x <= 59)

  /**
    * Parses a timestamp
    * @example parsePrint(timeP, "10:30") 
    */
  val timeP: Parser[Time] = (
    hourP <* delimiterP(':'),
    minuteP
  ).mapN(Time)

  /**
    * Parses a time range
    * For example "" turns into:
    * 
    * @example parsePrint(timeRangeP, "10:30 - 11:30")
    */
  val timeRangeP: Parser[TimeRange] = (
    timeP <* delimiterP('-'),
    timeP
  ).mapN(TimeRange)

  /**
    * Parses multiple time ranges
    * @example parsePrint(timeRangesP, "10:30 - 11:30, 12:30-14:00")
    */
  val timeRangesP: Parser[NonEmptyList[TimeRange]] =
    timeRangeP.repSep(delimiterP(','))

  /**
    * Parses a single OSM opening hour record
    * For example 
    * @example parsePrint(recordP, "Mo-We 10:30 - 11:30, 12:45  - 14:00")
    */
  val recordP: Parser[Record] = (
    dayRangeP <* whitespace0P,
    timeRangesP
  ).mapN(Record)

  /**
    * Parses mulitple OSM open hour records, delimeted by ';'
    * @example parsePrint(recordsP, "Mo-Fr 08:00-12:00,13:00-17:30; Sa-Su 10:00 - 14:00")
    */
  val recordsP: Parser[NonEmptyList[Record]] =
    recordP
      .repSep(delimiterP(';'))
      .surroundedBy(whitespace0P)
}

import OSMParser._

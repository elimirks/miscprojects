import atto._
import Atto._
import cats.implicits._
import Function.const

/**
  * Simplified OSM format parser.
  */
object OSMParser {
  sealed abstract class Day extends Product with Serializable
  final case object Monday extends Day
  final case object Tuesday extends Day
  final case object Wednesday extends Day
  final case object Thursday extends Day
  final case object Friday extends Day
  final case object Saturday extends Day
  final case object Sunday extends Day

  final case class Time(hour: Int, minute: Int) extends Serializable

  final case class DayRange(start: Day, end: Day) extends Serializable
  final case class TimeRange(start: Time, end: Time) extends Serializable

  final case class Record(
    days: DayRange,
    timeRanges: Seq[TimeRange],
  ) extends Serializable

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
    int.filter(value => value >= 0 && value <= 60)

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

object Main extends App {
  val sample: String = "Mo-Fr 08:00-12:00,13:00-17:30;Sa-Su 10:00-14:00"
  pprint.pprintln(OSMParser.parse(sample))
}

package scatto

import cats.data.NonEmptyList

/**
  * Simplified OSM format
  */

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
  timeRanges: NonEmptyList[TimeRange],
) extends Serializable

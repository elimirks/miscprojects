import scala.util.Try

import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}
import cats.data.NonEmptyList

import Helper.parsePrint

object Example2 {
  final case class Time(
    hour: Int,
    minute: Int
  )

  /**
    * The `Numbers` file in cats-parse has some definitons for parsing numbers
    * But we only want small unsigned ints, so this is parsing more than we need
    * @example parsePrint(simpleUintP, "10")
    * @example parsePrint(simpleUintP, "36")
    * @example parsePrint(simpleUintP, "-12")
    */
  val simpleUintP: Parser[BigInt] =
    Numbers.bigInt

  /**
    * `.toInt` will throw an exception if the int is out of bounds
    * This function instead returns `None` on failure
    */
  def safeToInt(value: String): Option[Int] =
    Try(value.toInt).toOption

  /**
    * `Numbers` also gives us the `digits` parser, which parses at least one number
    * It returns a string though, so we use `mapFilter` to try turning it into an int
    * `mapFilter` will first map, then filter out `None` values from options
    * @example parsePrint(uintP, "0003")
    * @example parsePrint(uintP, "2398473209740923743290478")
    */
  val uintP: Parser[Int] =
    Numbers.digits.mapFilter(safeToInt)

  /**
    * We can use the `filter` combinator to fail parsers given some predicate
    * @example parsePrint(hourP, "36")
    * @example parsePrint(hourP, "12")
    */
  val hourP: Parser[Int] =
    uintP.filter(x => x >= 0 && x <= 23)

  val minuteP: Parser[Int] =
    uintP.filter(x => x >= 0 && x <= 59)

  /**
    * Similar to dayRangeP in Example1
    */
  val timeP: Parser[Time] = (
    hourP <* Parser.char(':'),
    minuteP
  ).mapN((hour, minute) => Time(hour, minute))

  /**
    * The `rep` combinator allows parsing the same thing multiple times in a row
    * NonEmptyList is like Seq, but it can never be empty.
    * @see https://typelevel.org/cats/datatypes/nel.html
    * @example parsePrint(simpleTimesP, "10:3011:30")
    */
  val simpleTimesP: Parser[NonEmptyList[Time]] =
    timeP.rep

  /**
    * `repSep` is the same as `rep`, except with a delimiter between each item 
    * @example parsePrint(timesP, "10:30,11:30")
    */
  val timesP: Parser[NonEmptyList[Time]] =
    timeP.repSep(Parser.char(','))

  /**
    * Using `Parser0` let's you parse empty values
    * Use this with caution, it's probably not what you want to do!
    * @example parsePrint(times0P, "10:30,11:30")
    * @example parsePrint(times0P, "")
    */
  val times0P: Parser0[Seq[Time]] =
    timeP.repSep0(Parser.char(','))
}

import Example2._

package scatto

import scala.util.Try
import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}
import cats.data.NonEmptyList

/**
  * scatto.Example2.simpleHourP.parseAll("10")
  * scatto.Example2.simpleHourP.parseAll("36")
  * scatto.Example2.hourP.parseAll("36")
  * scatto.Example2.simpleTimesP.parseAll("10:3011:30")
  * scatto.Example2.timesP.parseAll("10:30,11:30")
  * scatto.Example2.times0P.parseAll("10:30,11:30")
  * scatto.Example2.times0P.parseAll("")
  */
object Example2 {
  final case class Time(
    hour: BigInt,
    minute: BigInt
  )

  /**
    * The `Numbers` file in cats-parse has some definitons for parsing numbers
    */
  val simpleHourP: Parser[BigInt] =
    Numbers.bigInt

  /**
    * We can use the `filter` combinator to fail parsers given some predicate
    */
  val hourP: Parser[BigInt] =
    Numbers.bigInt.filter(x => x >= 0 && x <= 23)

  val minuteP: Parser[BigInt] =
    Numbers.bigInt.filter(x => x >= 0 && x <= 59)

  /**
    * Similar to dayRangeP in Example1
    */
  val timeP: Parser[Time] = for {
    hour   <- hourP <* Parser.char(':')
    minute <- minuteP
  } yield Time(hour, minute)

  /**
    * The `rep` combinator allows parsing the same thing multiple times in a row
    */
  val simpleTimesP: Parser[NonEmptyList[Time]] =
    timeP.rep

  /**
    * `repSep` is the same as `rep`, except with a delimiter between each item 
    */
  val timesP: Parser[NonEmptyList[Time]] =
    timeP.repSep(Parser.char(','))

  /**
    * Using `Parser0` let's you parse empty values
    * Use this with caution, it's probably not what you want to do!
    */
  val times0P: Parser0[Seq[Time]] =
    timeP.repSep0(Parser.char(','))
}

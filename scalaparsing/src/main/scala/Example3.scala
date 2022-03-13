import scala.util.Try

import cats.implicits._
import cats.parse.{Parser0, Parser, Numbers}
import cats.data.NonEmptyList

import Helper.parsePrint

/**
  * Cats-parse defines 3 parser statuses:
  * 1. Success
  * 2. Epsilon failure,  when nothing has been parsed
  * 3. Arresting failure when something has been parsed, but there was a failure further down
  *
  * `Parser0` can be in any of these states
  * `Parser` can only have "success" or "arresting failure" statuses
  */
object Example3 {
  sealed abstract class Breakfast extends Product with Serializable

  final case class Waffle(
    withSyrup: Boolean = false
  ) extends Breakfast

  final case class Pancake(
    withSyrup: Boolean = false
  ) extends Breakfast

  /**
    * @example parsePrint(waffleP, "waffle")
    */
  val waffleP: Parser[Waffle] =
    Parser.string("waffle").as(Waffle())

  val syrupP: Parser[Unit] =
    Parser.string("syrup ")

  /**
    * What if we want to optionally parse something?
    * We can use `.?`
    * We have to use Parser0, since `.?` might return nothing
    *
    * @example parsePrint(optionalSyrupP, "syrup ")
    * @example parsePrint(optionalSyrupP, "maple syrup ")
    */
  val optionalSyrupP: Parser0[Option[Unit]] =
    syrupP.?

  /**
    * What if we want to optionally parse a Waffle, with a suffix " with syrup"
    * We can use the `.?` method to optionally parse something
    *
    * @example parsePrint(waffleIgnoreSyrupP, "waffle")
    * @example parsePrint(waffleIgnoreSyrupP, "syrup waffle")
    * @example parsePrint(waffleIgnoreSyrupP, "maple syrup waffle")
    */
  val waffleIgnoreSyrupP: Parser0[Waffle] =
    optionalSyrupP *> waffleP

  /**
    * Suppose we want the result after applying `.?`
    *
    * @example parsePrint(waffleWithSyrup0P, "waffle")
    * @example parsePrint(waffleWithSyrup0P, "syrup waffle")
    * @example parsePrint(waffleWithSyrup0P, "maple syrup waffle")
    */
  val waffleWithSyrup0P: Parser0[Waffle] = {
    val hasSyrupP: Parser0[Boolean] =
      optionalSyrupP.map(_.nonEmpty)

    (hasSyrupP <* waffleP)
      .map(hasSyrup => Waffle(withSyrup = hasSyrup))
  }

  /**
    * If you want to avoid Parser0, you can do something like this
    *
    * @example parsePrint(waffleWithSyrupP, "waffle")
    * @example parsePrint(waffleWithSyrupP, "syrup waffle")
    */
  val waffleWithSyrupP: Parser[Waffle] =
    Parser.oneOf(List(
      (syrupP *> waffleP).as(Waffle(true)),
      waffleP.as(Waffle(false))
    ))

  val pancakeWithSyrupP: Parser[Pancake] =
    Parser.oneOf(List(
      (syrupP *> Parser.string("pancake")).as(Pancake(true)),
      Parser.string("pancake").backtrack.as(Pancake(false))
    ))

  /**
    * @example parsePrint(brokenBreakfastP, "syrup pancake")
    * @example parsePrint(brokenBreakfastP, "pancake")
    * @example parsePrint(brokenBreakfastP, "waffle")
    *
    * Uh oh. "syrup " was parsed by `pancakeP`
    * But that parsed expects to see "pancake" after seeing "syrup"!
    * So the waffleP parser never gets called
    * @example parsePrint(brokenBreakfastP, "syrup waffle")
    */
  val brokenBreakfastP: Parser[Int] =
    pancakeWithSyrupP.as(0) <+> waffleWithSyrupP.as(2)

  /**
    * Use `.backtrack` to allow the parsing to "rewind" and avoid this issue
    * For best performance, avoid this when possible
    *
    * @example parsePrint(breakfastP, "syrup pancake")
    * @example parsePrint(breakfastP, "pancake")
    * @example parsePrint(breakfastP, "waffle")
    * @example parsePrint(breakfastP, "syrup waffle")
    */
  val breakfastP: Parser[Int] =
    Parser.oneOf(List(
      pancakeWithSyrupP.backtrack.as(0),
      waffleWithSyrupP.as(2)
    )).backtrack
}

import Example3._

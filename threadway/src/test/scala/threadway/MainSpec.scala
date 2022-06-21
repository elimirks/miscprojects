package threadway

import scala.io.Source
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Rules
class MainSpec extends AnyFlatSpec with Matchers {
  "The thing" should "say hello" in {
    Main.updateWorld(Source.fromFile("src/test/resources/blinker.txt"))
  }

  "neighborCount" should "Count the neighbors of the middle row" in {
    Main.neighborCount(
      Array(false, false, false),
      Array(true, true, true),
      Array(false, false, false),
      0
    ) shouldEqual 1

    Main.neighborCount(
      Array(false, true, false),
      Array(true, true, true),
      Array(false, false, false),
      1
    ) shouldEqual 3
  }

  "processRow" should "oscillate a blinker" in {
    Main.processRow(
      Array(false, false, false),
      Array(true, true, true),
      Array(false, false, false),
    ) shouldEqual "-x-"

    Main.processRow(
      Array(false, true, false),
      Array(false, true, false),
      Array(false, true, false),
    ) shouldEqual "xxx"
  }
}

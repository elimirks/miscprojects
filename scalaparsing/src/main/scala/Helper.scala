import cats.parse.{Parser0}
import pprint.pprintln

object Helper {
  def parsePrint(parser: Parser0[_], input: String) {
    pprintln(parser.parseAll(input))
  }
}

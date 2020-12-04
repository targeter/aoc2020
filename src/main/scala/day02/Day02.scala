package day02

import shared._
import cats.implicits._

case object Day02 extends AocTools(day = 2) {

  case class PassWithPolicy(min: Int, max: Int, char: Char, password: String) {
    private lazy val charCount: Int = password.count(_ == char)
    private lazy val charA: Char = password.charAt(min - 1)
    private lazy val charB: Char = password.charAt(max - 1)

    def charCountMatches: Boolean = charCount >= min && charCount <= max
    def charPosMatches: Boolean = (charA == char) != (charB == char)
  }

  object PassWithPolicy {
    private val r = """(\d+)-(\d+) (\w): (.*)""".r
    def apply(line: String): PassWithPolicy =
      line match {
        case r(min, max, char, pw) => PassWithPolicy(min.toInt, max.toInt, char.head, pw.trim)
      }
  }

  private def parseLines(input: Seq[String] = inputLines): Seq[PassWithPolicy] = input.map(PassWithPolicy(_))

  def passwordsValid1(input: Seq[String] = inputLines): Int = {
    parseLines(input).count(_.charCountMatches)
  }

  def passwordsValid2(input: Seq[String] = inputLines): Int = {
    parseLines(input).count(_.charPosMatches)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + passwordsValid1()) // 519
    println("Step 2: " + passwordsValid2()) // 708

  }

}

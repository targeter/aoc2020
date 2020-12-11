package aoc2020

import shared._

import scala.util.matching.Regex

case object Day07 extends AocTools(day = 7) {

  type Rules = Map[String, Map[String, Int]]

  val colorToContents: Regex = "(.*) bags contain (.+)".r
  val content: Regex = "(\\d)+ (.+) bags?\\.?".r

  def parse(input: Seq[String]): Rules = {
    input.map {
      case colorToContents(key, contents) =>
        val shouldHave = contents
          .split(",")
          .map(_.trim)
          .collect {
            case content(nr, color) => color -> nr.toInt
          }
          .toMap
        (key, shouldHave)
    }
  }.toMap

  def canReach(from: String, target: String)(implicit rules: Rules): Boolean = {
    val rule = rules(from)
    rule.contains(target) || rule.keys.exists(canReach(_, target))
  }

  def countChildren(from: String)(implicit rules: Rules): Int = {
    rules(from).map {
      case (color, number) => number + (countChildren(color) * number)
    }.sum
  }

  def main(args: Array[String]): Unit = {
    implicit val rules = parse(inputLines)
    println("Step 1: " + rules.keys.count(canReach(_, "shiny gold")))
    println("Step 2: " + countChildren("shiny gold"))
  }

}

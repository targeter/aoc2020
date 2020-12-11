package aoc2020

import shared._

import scala.util.matching.Regex

case object Day04 extends AocTools(day = 4) {

  private val heightRegex: Regex = "(\\d\\d\\d*)(cm|in)".r
  private val validEyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  private def numberInRange(min: Int, max: Int): String => Boolean = (in: String) => in.matches("^\\d{4}$") && in.toInt >= min && in.toInt <= max

  def heightValid(in: String): Boolean =
    in match {
      case heightRegex(num, "cm") if num.toInt >= 150 && num.toInt <= 193 => true;
      case heightRegex(num, "in") if num.toInt >= 59 && num.toInt <= 76   => true;
      case _                                                              => false
    }

  private val fields: Map[String, String => Boolean] = Set(
    "byr" -> numberInRange(1920, 2002),
    "iyr" -> numberInRange(2010, 2020),
    "eyr" -> numberInRange(2020, 2030),
    "hgt" -> heightValid _,
    "hcl" -> "#[0-9|a-f]{6}".r.matches _,
    "ecl" -> validEyeColors.contains _,
    "pid" -> "^\\d{9}$".r.matches _,
    "cid" -> ((_:String) => true),
  ).toMap

  def parse(input: String): Seq[Map[String, String]] = {
    input
      .split("\n\n")
      .map(_.split("\\s+").toList)
      .map(_.map(_.split(":")).map(l => l(0) -> l(1)).toMap)
      .toList
  }

  def isValid(input: Set[String]): Boolean = (fields.keySet.diff(input) - "cid").isEmpty

  def step1(): Int = parse(inputBlob).count(m => isValid(m.keySet))
  def step2(): Int =
    parse(inputBlob)
      .filter(m => isValid(m.keySet))
      .count(_.forall {
        case (field, value) => fields(field)(value)
      })

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1())
    println("Step 2: " + step2())
  }

}

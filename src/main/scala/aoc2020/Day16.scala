package aoc2020

import shared._

import scala.annotation.tailrec
import scala.collection.Set
import scala.util.matching.Regex

case object Day16 extends AocTools(day = 16) {

  val RulePattern: Regex = """(.+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  case class Rule(field: String, firstRange: Range, secondRange: Range) {
    def valid(input: Int): Boolean = firstRange.contains(input) || secondRange.contains(input)
  }

  def parseFieldRules(input: Seq[String]): Seq[Rule] = {
    input.map {
      case RulePattern(field, i1, i2, i3, i4) => Rule(field, i1.toInt to i2.toInt, i3.toInt to i4.toInt)
    }

  }

  def step1(rules: Seq[Rule], tickets: Seq[Seq[Int]]) = {
    val invalidFields = for {
      ticket <- tickets
      field <- ticket
      if !rules.exists(_.valid(field))
    } yield field

    invalidFields.sum
  }

  @tailrec
  def resolve(remaining: Seq[(Int, Set[Rule])], result: Map[String, Int]): Map[String, Int] = {
    val entriesWithSingleValidRule = remaining.collect {
      case (idx, rules) if rules.size == 1 => (idx, rules.head)
    }

    val (matchingIndexes, matchingRules) = entriesWithSingleValidRule.unzip

    val newRemainingRules: Seq[(Int, Set[Rule])] = remaining
      .collect {
        case (index, rules) if !matchingIndexes.contains(index) => (index, rules.diff(matchingRules.toSet))
      }

    val newResults = result ++ entriesWithSingleValidRule.map { case (i, r) => r.field -> i }

    if (newRemainingRules.isEmpty) newResults else resolve(newRemainingRules, newResults)
  }

  def filterValidTickets(rules: Seq[Rule], tickets: Seq[Seq[Int]]): Seq[Seq[Int]] = tickets.filter(t => t.forall(i => rules.exists(_.valid(i))))

  def step2(rules: Seq[Rule], tickets: Seq[Seq[Int]], myTicket: Seq[Int]): Map[String, Int] = {
    val validTicketFieldsIndexed = filterValidTickets(rules, tickets).transpose.zipWithIndex

    val validRulesByIndex: Seq[(Int, Set[Rule])] = validTicketFieldsIndexed.map {
      case (values, index) => index -> rules.filter(r => values.forall(r.valid)).toSet
    }

    resolve(validRulesByIndex, Map.empty).view.mapValues(myTicket).toMap
  }

  def main(args: Array[String]): Unit = {

    val my = inputLines(22).split(",").map(_.toInt).toList
    val rules = parseFieldRules(inputLines.take(20))
    val tickets = inputLines.drop(25).map(_.split(",").toSeq.map(_.toInt))

    println("Step 1: " + step1(rules, tickets)) //30869
    println("Step 2: " + step2(rules, tickets, my).collect {
      case (field, value) if field.startsWith("departure") => value.toLong
    }.product) // 4381476149273
  }
}

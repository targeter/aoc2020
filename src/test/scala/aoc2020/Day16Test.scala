package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day16Test extends AnyFunSuite with Matchers with OptionValues {

  import aoc2020.Day16._

  val rulesInput = """class: 1-3 or 5-7
                     |row: 6-11 or 33-44
                     |seat: 13-40 or 45-50""".stripMargin.split("\n").toSeq

  val ticketsInput: Seq[Seq[Int]] = Seq(Seq(7, 3, 47), Seq(40, 4, 50), Seq(55, 2, 20), Seq(38, 6, 12))

  test("test") {
    val rules = parseFieldRules(rulesInput)
    ticketsInput.head.forall(i => rules.exists(_.valid(i))) must be(true)
    ticketsInput(1).forall(i => rules.exists(_.valid(i))) must be(false)
    ticketsInput(2).forall(i => rules.exists(_.valid(i))) must be(false)
    ticketsInput(3).forall(i => rules.exists(_.valid(i))) must be(false)

    ticketsInput(1).filter(i => !rules.exists(_.valid(i))) must be(Seq(4))
    ticketsInput(2).filter(i => !rules.exists(_.valid(i))) must be(Seq(55))
    ticketsInput(3).filter(i => !rules.exists(_.valid(i))) must be(Seq(12))

    step1(parseFieldRules(rulesInput), ticketsInput) must be(71)
    filterValidTickets(rules, ticketsInput) must be(Seq(ticketsInput.head))
    ticketsInput.transpose.head must be(Seq(7, 40, 55, 38))

  }

  test("step2") {
    val rulesIn = """class: 0-1 or 4-19
                  |row: 0-5 or 8-19
                  |seat: 0-13 or 16-19
                  |""".stripMargin.split("\n").toSeq
    val nearby = Seq(Seq(3, 9, 18), Seq(15, 1, 5), Seq(5, 14, 9))
    val rules = parseFieldRules(rulesIn)
    val ticket = Seq(11, 12, 13)

    step2(rules, nearby, ticket) must be(Map("class" -> 12, "row" -> 11, "seat" -> 13))
  }

}

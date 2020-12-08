package day08

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day08Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import Day08._

  val example = """nop +0
                  |acc +1
                  |jmp +4
                  |acc +3
                  |jmp -3
                  |acc -99
                  |acc +1
                  |jmp -4
                  |acc +6""".stripMargin.split('\n').toList

  test("parse") {
    val prog = parse(example).toList
    val result = Program(prog).run
    result must be(Looped(5))
  }

  test("findPatch") {
    val prog = parse(example).toList
    findPatch(prog).value must be(8)
  }

}

package aoc2020

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.{Inspectors, OptionValues}

class Day14Test extends AnyFunSuite with Matchers with OptionValues with Inspectors {

  import aoc2020.Day14._

  val example = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
                  |mem[8] = 11
                  |mem[7] = 101
                  |mem[8] = 0""".stripMargin.split("\n").toList

  test("mask") {
    val mask = Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    println(mask.pos)
    println(mask.neg)
    mask(101) must be(101)
    mask(11) must be(73)
    mask(0) must be(64)

  }

  test("prog")  {
    val program = parse(example)
    program must be(List(SetMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"), Write(8, 11), Write(7, 101), Write(8, 0)))

    val c = Computer(Map[BigInt, BigInt](), Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"), program)
    c.next.next must be(Computer(Map(BigInt(8) -> BigInt(73)), Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"), program.tail.tail))

    c.run must be(165)
  }

}

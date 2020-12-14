package aoc2020

import shared._

case object Day14 extends AocTools(day = 14) {

  sealed trait Instruction
  case class SetMask(value: String) extends Instruction
  case class Write(pos: Int, value: BigInt) extends Instruction

  val allOnes = BigInt("68719476735")

  case class Mask(mask: String) {

    val pos: BigInt = BigInt(mask.replaceAll("X", "0"), 2)

    val neg: BigInt = BigInt(
      mask.map {
        case '0' => '1'
        case _   => '0'
      },
      2
    ) ^ allOnes

    def apply(value: BigInt): BigInt = (value | pos) & neg
  }

  case class Computer(memory: Map[BigInt, BigInt], mask: Mask, program: List[Instruction]) {
    def next: Computer =
      program match {
        case SetMask(mask) :: rest     => copy(mask = Mask(mask), program = rest)
        case Write(pos, value) :: rest => copy(memory = memory.updated(pos, mask(value)), program = rest)
      }

    def run: BigInt = if (program.isEmpty) memory.values.sum else next.run
  }

  object Computer {
    def apply(input: List[String]): Computer = Computer(Map.empty, Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"), parse(input))
  }

  private val MemInstruction = """mem\[(\d+)] = (\d+)""".r
  private val MaskInstruction = """mask = (\w+)""".r
  def parse(input: List[String]): List[Instruction] = {
    input.map {
      case MemInstruction(pos, value) => Write(pos.toInt, BigInt(value))
      case MaskInstruction(mask)      => SetMask(mask)
    }
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + Computer(inputLines).run) //10885823581193
    println("Step 2: ")
  }

}

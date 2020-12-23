package aoc2020

import shared._

import scala.util.parsing.combinator.JavaTokenParsers

case object Day18 extends AocTools(day = 18) with JavaTokenParsers {
  sealed trait Operand {
    def resolve: Long
    def add(oper: Operand): Op
    def mul(oper: Operand): Op
  }
  case class Num(value: Long) extends Operand {
    def add(num: Operand) = Op(this, Add, num)
    def mul(num: Operand) = Op(this, Mul, num)
    override def resolve: Long = value.toInt
  }

  sealed trait Operator {
    def apply(left: Long, right: Long): Long
  }
  object Add extends Operator {
    override def apply(left: Long, right: Long): Long = left + right
  }
  object Mul extends Operator {
    override def apply(left: Long, right: Long): Long = left * right
  }

  case class Op(left: Operand, operator: Operator, right: Operand) extends Operand {
    def resolve: Long = operator(left.resolve, right.resolve)

    def add(num: Operand) = Op(this, Add, num)
    def mul(num: Operand) = Op(this, Mul, num)
  }

  object Parser extends JavaTokenParsers {
    def num = "\\d+".r ^^ (_.toLong)
    def simple = num | "(" ~> expr <~ ")"
    def expr: Parser[Long] =
      (simple ~ rep(("+" | "*") ~ simple)) ^^ {
        case a ~ rest =>
          rest.foldLeft(a) {
            case (a, "+" ~ b) => a + b
            case (a, "*" ~ b) => a * b
          }
      }

    def parse(input: String): ParseResult[Long] = parseAll(expr, input)

  }

  object Parser2 extends JavaTokenParsers {
    def simple: Parser[Operand] = "\\d+".r ^^ ((v: String) => Num(v.toLong)) | "(" ~> expr <~ ")"

    def factor: Parser[Operand] = {
      val function: (Operand, Operand) => Op = Op.apply(_, Add, _)
      chainl1(simple, "+" ^^^ function)
    }
    def expr: Parser2.Parser[Operand] = {
      val function: (Operand, Operand) => Op = Op(_, Mul, _)
      chainl1(factor, "*" ^^^ function)
    }

    def parse(input: String): Parser2.ParseResult[Long] = parseAll(expr, input).map(_.resolve)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + inputLines.map(Parser.parseAll(Parser.expr, _).get).sum)
    println("Step 2: " + inputLines.map(Parser2.parse(_).get).sum)
  }

}

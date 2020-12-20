package aoc2020

import shared._

import scala.collection.Set
import scala.util.parsing.combinator.JavaTokenParsers

case object Day18 extends AocTools(day = 18) with JavaTokenParsers {
//  val NumberPattern = """\d""".r
//  val OperationPattern = """(\d+) ([*|+]) (\d+)""".r
//
//  sealed trait Operand {
//    def resolve: Long
//    def add(oper: Operand): Op
//    def mul(oper: Operand): Op
//  }
//  case class Num(value: Long) extends Operand {
//    def add(num: Operand) = Op(this, Add, num)
//    def mul(num: Operand) = Op(this, Mul, num)
//    override def resolve: Long = value.toInt
//  }
//
//  sealed trait Operator {
//    def apply(left: Long, right: Long): Long
//  }
//  object Add extends Operator {
//    override def apply(left: Long, right: Long): Long = left + right
//  }
//  object Mul extends Operator {
//    override def apply(left: Long, right: Long): Long = left * right
//  }
//
//  case class Op(left: Operand, operator: Operator, right: Operand) extends Operand {
//    def resolve: Long = operator(left.resolve, right.resolve)
//
//    def add(num: Operand) = Op(this, Add, num)
//    def mul(num: Operand) = Op(this, Mul, num)
//  }

  object Parser extends JavaTokenParsers {
    def num = wholeNumber ^^ (_.toLong)
    def operand = num | "(" ~> expr <~ ")"
    def expr: Parser[Long] =
      (operand ~ rep(("+" | "*") ~ operand)) ^^ {
        case a ~ rest =>
          rest.foldLeft(a) {
            case (a, "+" ~ b) => a + b
            case (a, "*" ~ b) => a * b
          }
      }

    def parse(input: String) = parseAll(expr, input)

  }

  object Parser2 extends JavaTokenParsers {
    def num = wholeNumber ^^ (_.toLong)
    def operand = num | "(" ~> expr <~ ")"
    def expr: Parser[Long] =
      (operand ~ rep("+" ~ operand)) ^^ {
        case a ~ rest =>
          rest.foldLeft(a) {
            case (a, "+" ~ b) => a + b
          }
      }

    def expr2 =
      (expr ~ rep("*" ~ expr)) ^^ {
        case a ~ rest =>
          rest.foldLeft(a) {
            case (a, "*" ~ b) => a * b
          }
      }

    def parse(input: String) = parseAll(expr2, input)

  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + inputLines.map(Parser.parseAll(Parser.expr, _).get).sum)
    println("Step 2: " + inputLines.map(Parser2.parse(_).getOrElse(0)))
  }

}

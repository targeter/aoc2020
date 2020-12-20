package aoc2020

import shared._

case object Day19 extends AocTools(day = 19) {
  import shared._

  val SingeCharPattern = "\"(.)\"".r
  val RefPattern = "(\\d+)".r
  val DoubleRefPattern = """(\d+) (\d+)""".r
  val OrPattern = """(\d+) \| (\d+)""".r
  val DoubleOrPattern = """(\d+) (\d+) \| (\d+) (\d+)""".r

  sealed trait Rule {
    def resolve(implicit rulesMap: Map[Int, Rule]): String
  }
  case class CharRule(c: String) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = c
  }
  case class Ref(i: Int) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = rulesMap(i).resolve
  }
  case class DoubleRef(i: Ref, j: Ref) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = i.resolve + j.resolve
  }
  case class RefOrRef(l: Ref, r: Ref) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = s"(?:${l.resolve}|${r.resolve})"
  }

  case class DoubleOrRef(l: DoubleRef, r: DoubleRef) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = s"(?:${l.resolve}|${r.resolve})"
  }

  case class Repeat(ref: Rule) extends Rule {
    override def resolve(implicit rulesMap: Map[Int, Rule]): String = s"(${ref.resolve})+"
  }

  def parseRules(input: Seq[String]): Map[Int, Rule] = {
    def parseRule(line: String): Rule = {
      line match {
        case SingeCharPattern(c)         => CharRule(c)
        case RefPattern(c)               => Ref(c.toInt)
        case DoubleRefPattern(a, b)      => DoubleRef(Ref(a.toInt), Ref(b.toInt))
        case OrPattern(a, b)             => RefOrRef(Ref(a.toInt), Ref(b.toInt))
        case DoubleOrPattern(a, b, c, d) => DoubleOrRef(DoubleRef(Ref(a.toInt), Ref(b.toInt)), DoubleRef(Ref(c.toInt), Ref(d.toInt)))
      }
    }

    input.map { line =>
      val Array(i, rule) = line.split(": ")
      i.toInt -> parseRule(rule)
    }.toMap
  }

  def matchLine(line: String)(implicit rulesMap: Map[Int, Rule]): Boolean = rulesMap(0).resolve.r.matches(line)

  def matchLine2(line: String, rulesMap: Map[Int, Rule]): Boolean = {
    def rule8: Rule =
      new Rule {
        override def resolve(implicit rulesMap: Map[Int, Rule]): String = {
          val orig = rulesMap(42).resolve
          s"""(?:$orig)+"""
        }
      }

    def rule11: Rule =
      new Rule {
        override def resolve(implicit rulesMap: Map[Int, Rule]): String = {
          val orig42 = rulesMap(42).resolve
          val orig31 = rulesMap(31).resolve
          val generatedRepetitions = (1 to 3).map(i => s"""$orig42{$i}$orig31{$i}""").mkString("|")
          s"""(?:$generatedRepetitions)"""
        }
      }

    implicit val newMap = rulesMap.updated(8, rule8).updated(11, rule11)

    newMap(0).resolve.r.matches(line)
  }

  def main(args: Array[String]): Unit = {

    val rulesInput = inputLines.take(139)
    val msgInput = inputLines.drop(140)

    implicit val rulesMap = parseRules(rulesInput)

    println("Step 1: " + msgInput.count(matchLine(_)))
    println("Step 2: " + msgInput.count(matchLine2(_, rulesMap))) //243

  }

}

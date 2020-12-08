package day08

import shared._

import scala.annotation.tailrec

case object Day08 extends AocTools(day = 8) {
  case class State(ptr: Int, acc: Int)

  sealed trait Patchable { this: Operation =>
    def patch: Operation = this
  }

  sealed trait Result
  case class Looped(acc: Int) extends Result
  case class Terminated(acc: Int) extends Result

  sealed trait Operation {
    def apply(state: State): State
  }

  case class Program(instructions: List[Operation]) {
    private var ran = Set(0)

    @tailrec
    private def execNext(newState: State): Result =
      newState match {
        case State(ptr, acc) if ptr >= instructions.length => Terminated(acc)
        case State(ptr, acc) if ran(ptr)                   => Looped(acc)
        case State(ptr, _) =>
          ran = ran + ptr
          execNext(instructions(ptr)(newState))
      }

    def run: Result = execNext(instructions.head(State(0, 0)))
  }

  case class Acc(arg: Int) extends Operation {
    override def apply(state: State): State = state.copy(ptr = state.ptr + 1, acc = state.acc + arg)
  }

  case class Jmp(arg: Int) extends Operation with Patchable {
    override def apply(state: State): State = state.copy(ptr = state.ptr + arg)

    override def patch: Nop = Nop(arg)
  }

  case class Nop(arg: Int) extends Operation with Patchable {
    override def apply(state: State): State = state.copy(ptr = state.ptr + 1)

    override def patch: Jmp = Jmp(arg)
  }

  def parse(lines: Seq[String]): Seq[Operation] =
    lines.map { l =>
      l.split(' ').toList match {
        case "acc" :: arg :: Nil => Acc(arg.toInt)
        case "jmp" :: arg :: Nil => Jmp(arg.toInt)
        case "nop" :: arg :: Nil => Nop(arg.toInt)
      }
    }

  def findPatch(instructions: List[Operation]): Option[Int] =
    instructions.zipWithIndex.view
      .collect {
        case (op: Operation with Patchable, idx) => Program(instructions.updated(idx, op.patch)).run
      }
      .collectFirst {
        case Terminated(result) => result
      }

  def main(args: Array[String]): Unit = {
    val instructions: List[Operation] = parse(inputLines).toList
    println("Step 1: " + Program(instructions).run)
    println("Step 2: " + findPatch(instructions))
  }

}

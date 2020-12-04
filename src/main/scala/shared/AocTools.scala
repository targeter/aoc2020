package shared

import scala.io.Source

abstract class AocTools(day: Int) {

  def inputLines: List[String] = InputGetter.get(day).toList

  def inputInts: List[Int] = inputLines.map(_.toInt)

  def inputBlob = inputLines.mkString("\n")
}

package object ops {

  implicit class SeqOps[T](seq: Seq[T]) {
    def rotate(n: Int): Seq[T] = seq.drop(n) ++ seq.take(n)
  }
}

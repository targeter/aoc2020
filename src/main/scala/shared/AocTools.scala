package shared

abstract class AocTools(day: Int) {

  def inputLines: List[String] = InputGetter.get(day).toList

  def inputLineInts: Seq[Int] = inputLines.flatMap(_.split(",").map(_.toInt)).toVector

  def inputInts: List[Int] = inputLines.map(_.toInt)

  def inputBlob = inputLines.mkString("\n")
}

package object ops {

  implicit class SeqOps[T](seq: Seq[T]) {
    def rotate(n: Int): Seq[T] = seq.drop(n) ++ seq.take(n)
  }

  implicit class StringOps[T](input: String) {
    def splitLines = input.split("\n").toList
    def asInts: List[Int] = splitLines.map(_.toInt)
  }
}

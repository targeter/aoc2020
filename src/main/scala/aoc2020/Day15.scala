package aoc2020

import shared._

case object Day15 extends AocTools(day = 15) {

  def step1(input: Seq[Int]) = LazyList.iterate(input)(spokenInTurn)

  def spokenInTurn(turns: Seq[Int]) = {
    val lastIndexOfLast = turns.dropRight(1).lastIndexOf(turns.last)
    val nextNum = if(lastIndexOfLast == -1) 0 else turns.length - lastIndexOfLast - 1
    turns :+ nextNum
  }

  def spokenInTurn2(cache:Map[Int, Int], turn: Int, last: Int, until: Int):Int = {
    val newLast = cache.get(last).map(turn - _).getOrElse(0)
    if(turn == until) last
    else spokenInTurn2(cache.updated(last, turn), turn + 1, newLast, until)

  }

  def main(args: Array[String]): Unit = {

    println("Step 1: " + step1(inputLineInts)(2020)(2019))
    val initialCache = inputLineInts.zipWithIndex.map{case (k, i) => k -> (i+1)}.toMap // 755
    println("Step 2: " + spokenInTurn2(initialCache, inputLineInts.size, inputLineInts.last, 30000000)) //11962
  }
}

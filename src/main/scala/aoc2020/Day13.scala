package aoc2020

import shared.{AocTools, ChineseRemainder}

case object Day13 extends AocTools(day = 13) {

  def step1(input: Seq[String]) = {
    val earliest = input.head.toInt
    val busIds = input.last.split(',').filter(_ != "x").flatMap(_.toIntOption)

    val waitTimesPerBus = busIds.map { id =>
      val departureNum = (earliest / id) + 1
      val departureTime = id * departureNum
      val waitTime = departureTime - earliest
      id -> waitTime
    }

    val (winningBus, leastTTW) = waitTimesPerBus.minBy(_._2)

    println(s"Min wait $leastTTW for bus $winningBus")
    winningBus * leastTTW
  }

  def step2(input: Seq[String]): Option[Long] = {
    val busIds = input(1).split(",").toList
    val busIdsIdx = busIds.zipWithIndex.flatMap { case (bus, idx) => bus.toLongOption.map(_ -> idx.toLong) }

    val (busses, targets) = busIdsIdx.map {
      case (bus, idx) => bus -> (bus - idx)
    }.unzip

    ChineseRemainder.chineseRemainder(busses, targets)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputLines)) // 138
    println("Step 2: " + step2(inputLines)) //226845233210288
  }

}

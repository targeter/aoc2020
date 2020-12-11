package aoc2020

import shared._

import scala.collection.immutable.IntMap

case object Day10 extends AocTools(day = 10) {

  def deviceJolts(input: List[Int]): Int = input.max + 3

  def step1(input: List[Int]): Int = {
    val allDiffs = diffs(input)
    allDiffs.count(_ == 1) * allDiffs.count(_ == 3)
  }

  def diffs(input: List[Int]) = {
    (0 +: input.sorted :+ deviceJolts(input))
      .sliding(2)
      .collect {
        case List(a, b) => b - a
      }
      .toList
  }

  def step2(input: List[Int]): Long = {
    val cache = scala.collection.mutable.Map.empty[Long, Long]
    val allJolts = 0 +: input.sorted :+ deviceJolts(input)

    val joltMap = IntMap.from(allJolts.map { i =>
      i -> allJolts.filter { j => j < i && j >= i - 3 }
    })

    def count(jolt: Int): Long =
      if (jolt == 0) 1
      else cache.getOrElseUpdate(jolt, joltMap(jolt).map(count).sum)

    count(deviceJolts(input))
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + step1(inputInts)) // 1690
    println("Step 2: " + step2(inputInts)) // 5289227976704
  }

}

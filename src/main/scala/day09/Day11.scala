package day09

import shared._

import scala.collection.immutable.LazyList
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParMap

case object Day11 extends AocTools(day = 11) {

  type Plan = ParMap[Position, Char]
  type Direction = Position => Position

  object Directions {
    val N: Direction = (pos: Position) => pos.copy(y = pos.y - 1)
    val W: Direction = (pos: Position) => pos.copy(x = pos.x - 1)
    val S: Direction = (pos: Position) => pos.copy(y = pos.y + 1)
    val E: Direction = (pos: Position) => pos.copy(x = pos.x + 1)

    val NW: Direction = (pos: Position) => pos.copy(y = pos.y - 1, x = pos.x - 1)
    val NE: Direction = (pos: Position) => pos.copy(y = pos.y - 1, x = pos.x + 1)
    val SE: Direction = (pos: Position) => pos.copy(y = pos.y + 1, x = pos.x - 1)
    val SW: Direction = (pos: Position) => pos.copy(y = pos.y + 1, x = pos.x + 1)

    val all = Seq(N, S, E, W, NW, NE, SW, SE)
  }

  case class Position(x: Int, y: Int) {
    def adjacent: Seq[Position] = Directions.all.map(_(this))
  }

  val Occupied = '#'
  val Empty = 'L'
  val Floor = '.'

  val Seat = Set(Occupied, Empty)

  def parse(input: List[String]): ParMap[Position, Char] =
    (for {
      y <- input.indices
      x <- input(y).indices
    } yield Position(x, y) -> input(y)(x)).toMap.par

  def findFinalState(plan: Plan, nextRound: Plan => Plan): Option[Plan] =
    LazyList
      .iterate(plan)(nextRound)
      .sliding(2)
      .collectFirst { case l if l.head == l.last => l.head }

  def finalOccupied(input: List[String], next: Plan => Plan): Option[Int] =
    findFinalState(parse(input), next).map(_.values.count(_ == Occupied))

  implicit class PlanOps(plan: Plan) {

    def adjacent(pos: Position): Seq[Char] = pos.adjacent.flatMap(plan.get)

    def firstVisibleSeats(pos: Position): Seq[Char] = Directions.all.flatMap(firstVisbleSeat(pos, _))

    def firstVisbleSeat(pos: Position, direction: Direction): Option[Char] =
      LazyList
        .iterate(direction(pos))(direction)
        .map(plan.get)
        .dropWhile(_.contains(Floor))
        .headOption
        .flatten
        .filter(Seat)

    def countAdjacentOccupied(pos: Position): Int = adjacent(pos).count(_ == Occupied)

    def countVisibleOccupied(pos: Position): Int = firstVisibleSeats(pos).count(_ == Occupied)

    def next: Plan =
      plan.map {
        case (pos, Occupied) if countAdjacentOccupied(pos) >= 4 => pos -> Empty
        case (pos, Empty) if countAdjacentOccupied(pos) == 0    => pos -> Occupied
        case other                                              => other
      }

    def next2: Plan =
      plan.map {
        case (pos, Occupied) if firstVisibleSeats(pos).count(_ == Occupied) >= 5 => pos -> Empty
        case (pos, Empty) if firstVisibleSeats(pos).count(_ == Occupied) == 0    => pos -> Occupied
        case other                                                               => other
      }

    def printIt: String = {
      val (xs, ys) = plan.keySet.flatMap(Position.unapply).unzip
      val maxX = xs.max
      val maxY = ys.max

      (0 to maxY).map(y => (0 to maxX).map(x => plan(Position(x, y))).mkString("")).mkString("\n")

    }
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + finalOccupied(inputLines, _.next)) // 2424
    println("Step 2: " + finalOccupied(inputLines, _.next2)) // 2208
  }

}

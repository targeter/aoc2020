package aoc2020

import aoc2020.Day11.{Direction, Directions, Position}
import shared._
import mouse.any._
case object Day12 extends AocTools(day = 12) {

  sealed trait Direction {
    import Direction.Directions
    // Divide by 90 to get number of places to shift. %4 to adjust overflow. +4 and %4 to adjust negative offset.
    def rotate(deg: Int): Direction = Directions((((Directions.indexOf(this) + deg / 90) % 4) + 4) % 4)
    def left(deg: Int): Direction = rotate(-deg)
    def right(deg: Int): Direction = rotate(deg)

    def forward(pos: Position, i: Int): Position = pos.north(i)
  }

  object Direction {
    private val Directions = List(North, East, South, West)
  }

  case object North extends Direction
  case object West extends Direction
  case object South extends Direction
  case object East extends Direction

  case class Position(e: Int, n: Int) {
    def north(i: Int) = copy(n = n + i)
    def south(i: Int) = copy(n = n - i)
    def east(i: Int) = copy(e = e + i)
    def west(i: Int) = copy(e = e - i)

  }

  case class Location(pos: Position, dir: Direction, waypoint: Position) {
    def north(i: Int): Location = copy(pos = pos.north(i))
    def south(i: Int): Location = copy(pos = pos.south(i))
    def east(i: Int): Location = copy(pos = pos.east(i))
    def west(i: Int): Location = copy(pos = pos.west(i))
    def left(i: Int): Location = copy(dir = dir.left(i))
    def right(i: Int): Location = copy(dir = dir.right(i))
    def forward(i: Int): Location = {
      dir match {
        case North => copy(pos = pos.north(i))
        case West  => copy(pos = pos.west(i))
        case South => copy(pos = pos.south(i))
        case East  => copy(pos = pos.east(i))
      }
    }
    def forward2(i: Int): Location = {
      val newPos = pos.copy(e = pos.e + (waypoint.e * i), n = pos.n + (waypoint.n * i))
      copy(pos = newPos)
    }

  }

  val start = Location(Position(0, 0), East, Position(10, 1))

  def step1(input: Seq[String]) = {
    input.map(s => s.head -> s.tail.toInt).foldLeft(start) {
      case (loc, (op, i)) =>
        (op match {
          case 'L' => loc.left(i)
          case 'R' => loc.right(i)
          case 'F' => loc.forward(i)
          case 'N' => loc.north(i)
          case 'S' => loc.south(i)
          case 'E' => loc.east(i)
          case 'W' => loc.west(i)
        })
    }
  }

  def step2(input: Seq[String]) = {
    input.map(s => s.head -> s.tail.toInt).foldLeft(start) {
      case (loc, (op, i)) =>
        (op match {
          case 'L' => {
            val newWay = (i, (loc.waypoint.e, loc.waypoint.n)) match {
              case (90, (e, n))  => Position(-1 * n, e)
              case (180, (e, n)) => Position(-1 * e, -1 * n)
              case (270, (e, n)) => Position(n, -1 * e)
            }
            loc.copy(waypoint = newWay)

          }
          case 'R' => {
            val newWay = (i, (loc.waypoint.e, loc.waypoint.n)) match {
              case (90, (e, n))  => Position(n, -1 * e)
              case (180, (e, n)) => Position(-1 * e, -1 * n)
              case (270, (e, n)) => Position(-1 * n, e)
            }
            loc.copy(waypoint = newWay)

          }
          case 'F' => loc.forward2(i)
          case 'N' => loc.copy(waypoint = loc.waypoint.north(i))
          case 'S' => loc.copy(waypoint = loc.waypoint.south(i))
          case 'E' => loc.copy(waypoint = loc.waypoint.east(i))
          case 'W' => loc.copy(waypoint = loc.waypoint.west(i))
        })
    }
  }

  def dist(position1: Position, position2: Position): Int = {
    (position1.e.abs + position2.e.abs) + (position1.n.abs + position2.n.abs)
  }

  def main(args: Array[String]): Unit = {
    println("Step 1: " + dist(start.pos, step1(inputLines).pos))
    println("Step 2: " + dist(start.pos, step2(inputLines).pos))
  }

}

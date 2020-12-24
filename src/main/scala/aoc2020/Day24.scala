package aoc2020

import aoc2020.Day24.HexCoordinate.ref
import shared._

import scala.util.matching.Regex

case object Day24 extends AocTools(day = 24) {
  import Side._
  import HexDirection._

  sealed trait Side { def flip: Side }
  object Side {
    case object White extends Side { val flip: Side = Black }
    case object Black extends Side { val flip: Side = White }
  }

  sealed trait HexDirection
  object HexDirection {
    case object E extends HexDirection
    case object SE extends HexDirection
    case object SW extends HexDirection
    case object W extends HexDirection
    case object NW extends HexDirection
    case object NE extends HexDirection
  }

  case class HexCoordinate(x: Int, y: Int) {
    def move(dir: HexDirection): HexCoordinate =
      dir match {
        case E  => copy(x = x - 1)
        case SE => copy(y = y - 1)
        case SW => copy(x = x + 1, y = y - 1)
        case W  => copy(x = x + 1)
        case NW => copy(y = y + 1)
        case NE => copy(x = x - 1, y = y + 1)
      }

    def move(dirs: Seq[HexDirection]): HexCoordinate = dirs.foldLeft(this)(_ move _)

    def adjacentCoordinates: Seq[HexCoordinate] = List(E, SE, SW, W, NW, NE).map(move)
  }

  object HexCoordinate {
    val ref: HexCoordinate = HexCoordinate(0, 0)
  }

  case class State(grid: Map[HexCoordinate, Side]) {
    def flipAt(steps: Seq[HexDirection]): State = {
      val target = ref.move(steps)
      copy(grid = grid.updatedWith(target)(_.map(_.flip).orElse(Some(Black))))
    }

    def countBlack: Int = grid.values.count(_ == Black)

    def countAdjacentBlack(coord: HexCoordinate): Int = coord.adjacentCoordinates.map(grid.getOrElse(_, White)).count(_ == Black)

    def maybeFlip(coord: HexCoordinate): Side = {
      val blackCount = countAdjacentBlack(coord)
      grid.getOrElse(coord, White) match {
        case Black if blackCount == 0 || blackCount > 2 => White
        case White if blackCount == 2                   => Black
        case curr                                       => curr
      }
    }

    def nextDay: State = {
      val maxX = grid.keySet.map(_.x).max + 1
      val minX = grid.keySet.map(_.x).min - 1
      val maxY = grid.keySet.map(_.y).max + 1
      val minY = grid.keySet.map(_.y).min - 1

      val newGrid = for {
        x <- minX to maxX
        y <- minY to maxY
        coord = HexCoordinate(x, y)
      } yield coord -> maybeFlip(coord)

      State(newGrid.toMap)
    }
  }

  object State {
    val empty: State = State(Map.empty[HexCoordinate, Side])
  }

  private val Directions: Regex = """((?:se|sw|ne|nw)|[ew])+?""".r
  def parseLine(input: String): Seq[HexDirection] =
    Directions
      .findAllIn(input)
      .map {
        case "e"  => E
        case "se" => SE
        case "sw" => SW
        case "w"  => W
        case "nw" => NW
        case "ne" => NE
      }
      .toSeq

  def initialState(input: List[String]): State = input.map(parseLine).foldLeft(State.empty)(_ flipAt _)

  def days(input: List[String]): LazyList[State] = LazyList.iterate(initialState(input))(_.nextDay)

  def main(args: Array[String]): Unit = {
    println("Step 1: " + initialState(inputLines).countBlack)
    println("Step 2: " + days(inputLines)(100).countBlack)

  }

}

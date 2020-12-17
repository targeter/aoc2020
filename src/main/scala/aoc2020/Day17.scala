package aoc2020

import shared._

import scala.collection.Set

case object Day17 extends AocTools(day = 17) {

  case class Cube[C <: Coordinate[C]](cubes: Map[C, CubeState]) {
    def step: Cube[C] = {
      Cube(cubesToCheck.map { coordinate =>
        val neighStates = coordinate.neighbors.map(cubes.getOrElse(_, Inactive)).count(_ == Active)

        val newState = cubes.getOrElse(coordinate, Inactive) match {
          case Active if neighStates == 2 || neighStates == 3 => Active
          case Active                                         => Inactive
          case Inactive if neighStates == 3                   => Active
          case Inactive                                       => Inactive
        }
        coordinate -> newState
      }.toMap)
    }

    def cubesToCheck: Set[C] = cubes.keySet.flatMap(_.neighbors)

    def active: Int = cubes.count(_._2 == Active)
  }

  sealed trait CubeState {
    def flip: CubeState
    def active: Boolean = this == Active
  }

  object CubeState {
    def apply(c: Char): CubeState = if (c == '#') Active else Inactive
  }

  case object Active extends CubeState {
    val flip: CubeState = Inactive
  }
  case object Inactive extends CubeState {
    val flip: CubeState = Active
  }

  trait Coordinate[C <: Coordinate[C]] { this: C =>
    def neighbors: Seq[C]
  }

  case class ThreeCoordinate(x: Int, y: Int, z: Int) extends Coordinate[ThreeCoordinate] {
    def neighbors: Seq[ThreeCoordinate] =
      for {
        newX <- Seq(x - 1, x, x + 1)
        newY <- Seq(y - 1, y, y + 1)
        newZ <- Seq(z - 1, z, z + 1)
        if (newX, newY, newZ) != (x, y, z)
      } yield ThreeCoordinate(newX, newY, newZ)

  }

  object ThreeCoordinate {
    implicit val factory: (Int, Int) => ThreeCoordinate = (x: Int, y: Int) => ThreeCoordinate(x, y, 0)
  }

  case class HyperCoordinate(x: Int, y: Int, z: Int, h: Int) extends Coordinate[HyperCoordinate] {
    def neighbors: Seq[HyperCoordinate] =
      for {
        newX <- Seq(x - 1, x, x + 1)
        newY <- Seq(y - 1, y, y + 1)
        newZ <- Seq(z - 1, z, z + 1)
        newH <- Seq(h - 1, h, h + 1)
        if (newX, newY, newZ, newH) != (x, y, z, h)
      } yield HyperCoordinate(newX, newY, newZ, newH)
  }

  object HyperCoordinate {
    implicit val factory: (Int, Int) => HyperCoordinate = (x: Int, y: Int) => HyperCoordinate(x, y, 0, 0)
  }

  def parse[C <: Coordinate[C]](input: List[String])(implicit factory: (Int, Int) => C) =
    input.zipWithIndex.flatMap {
      case (line, y) =>
        line.zipWithIndex.map {
          case (state, x) => factory(x, y) -> CubeState(state)
        }
    }.toMap

  def main(args: Array[String]): Unit = {
    val startCube = Cube(parse[ThreeCoordinate](inputLines))
    val cubeStates = LazyList.iterate(startCube)(_.step)
    println("Step 1: " + cubeStates(6).active)

    val startHyperCube = Cube(parse[HyperCoordinate](inputLines))
    val hyperCubeStates = LazyList.iterate(startHyperCube)(_.step)
    println("Step 2: " + hyperCubeStates(6).active)
  }
}

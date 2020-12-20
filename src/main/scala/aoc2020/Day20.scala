package aoc2020

import shared._
import mouse.any._

case object Day20 extends AocTools(day = 20) {
  import ops._

  val TileIdPattern = """Tile (\d+):""".r

  sealed trait Side

  object Side {
    case object Left extends Side
    case object Right extends Side
    case object Top extends Side
    case object Bottom extends Side
    def all = Seq(Top, Right, Bottom, Left)
  }

  case class Edge(value: String) extends AnyVal {
    def flipped = Edge(value.reverse)
  }

  case class Tile(id: Long, input: List[String]) {
    def borders: Map[Side, Edge] =
      Map(
        Side.Left -> Edge(input.transpose.head.mkString),
        Side.Right -> Edge(input.transpose.last.mkString),
        Side.Top -> Edge(input.head),
        Side.Bottom -> Edge(input.last)
      )

    def allEdges = borders.values.toList

    def flipped = copy(input = input.map(_.reverse))
  }

  def parse(input: String): List[Tile] = {
    val tiles = input.split("\n\n").map(_.splitLines)
    println(s"${tiles.length} pieces")
    tiles.map {
      case TileIdPattern(id) :: image => Tile(id.toLong, image)
    }.toList
  }

  def findCorners(tiles: List[Tile]) = {
    val edgeMap: Map[Edge, Set[Long]] = tiles
      .flatMap(t => t.allEdges.map(_ -> t.id))
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toMap
      .withDefault(_ => Set.empty[Long])

    tiles.filter { tile =>
      val edgeMatches = tile.allEdges.map(e => e-> edgeMap(e)).map{case(k,v) => k -> (v - tile.id)}
      val edgeMatchesWithFlipped = edgeMatches.collect {
        case (_, matches) if matches.nonEmpty => matches
        case (edge, _) => edgeMap(edge.flipped)
      }

      val edgesWithoutMatchingtile = edgeMatchesWithFlipped.count(_.isEmpty)
      edgesWithoutMatchingtile == 2

    }.map(_.id)
  }

  def main(args: Array[String]): Unit = {
    val step = findCorners(parse(inputBlob))

    println("Step 1: " + step.product)
    println("Step 2: ")

  }

}

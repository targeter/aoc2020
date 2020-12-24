package aoc2020

import aoc2020.Day24.HexCoordinate.ref
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class Day24Test extends AnyFunSuite with Matchers with OptionValues {

  import Day24._
  import HexDirection._
  import Side._
  import shared.ops._

  test("parse") {
    parseLine("esenee") must be(List(E, SE, NE, E))
  }

  test("move") {
    HexCoordinate(0, 0).move(List(E, SE, W)) must be(HexCoordinate(0, -1))
  }

  test("flip") {
    State.empty.flipAt(List(NW, W, SW, E, E)) must be(State(Map(ref -> Black)))
  }

  val example = """sesenwnenenewseeswwswswwnenewsewsw
                    |neeenesenwnwwswnenewnwwsewnenwseswesw
                    |seswneswswsenwwnwse
                    |nwnwneseeswswnenewneswwnewseswneseene
                    |swweswneswnenwsewnwneneseenw
                    |eesenwseswswnenwswnwnwsewwnwsene
                    |sewnenenenesenwsewnenwwwse
                    |wenwwweseeeweswwwnwwe
                    |wsweesenenewnwwnwsenewsenwwsesesenwne
                    |neeswseenwwswnwswswnw
                    |nenwswwsewswnenenewsenwsenwnesesenew
                    |enewnwewneswsewnwswenweswnenwsenwsw
                    |sweneswneswneneenwnewenewwneswswnese
                    |swwesenesewenwneswnwwneseswwne
                    |enesenwswwswneneswsenwnewswseenwsese
                    |wnwnesenesenenwwnenwsewesewsesesew
                    |nenewswnwewswnenesenwnesewesw
                    |eneswnwswnwsenenwnwnwwseeswneewsenese
                    |neswnwewnwnwseenwseesewsenwsweewe
                    |wseweeenwnesenwwwswnew
                    |""".stripMargin.splitLines

  test("step1") {
    initialState(example).countBlack must be(10)
  }

  test("step2") {
    initialState(example).nextDay.countBlack must be(15)
    initialState(example).nextDay.nextDay.countBlack must be(12)
    days(example)(3).countBlack must be(25)
    days(example)(100).countBlack must be(2208)
  }

}

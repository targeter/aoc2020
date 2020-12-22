package aoc2020

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import shared.ops._

class Day21Test extends AnyFunSuite with Matchers with OptionValues {

  import Day21._

  val example = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin.splitLines

  test("step1") {
    val puzzle = AllergensPuzzle(parse(example))
    puzzle.safeIngredients must be(Set("kfcds", "nhms", "sbzzf", "trh").map(Ingredient))
  }

}

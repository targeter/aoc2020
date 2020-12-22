package aoc2020

import shared._

import scala.annotation.tailrec

case object Day21 extends AocTools(day = 21) {

  case class Ingredient(value: String) extends AnyVal
  case class Allergen(value: String) extends AnyVal
  case class Food(ingredients: Set[Ingredient], allergens: Set[Allergen]) {}

  case class AllergensPuzzle(foods: Set[Food]) {
    val ingredientsSets: Set[Set[Ingredient]] = foods.map(_.ingredients)
    val allIngredients: Set[Ingredient] = foods.flatMap(_.ingredients)
    val allAllergens: Set[Allergen] = foods.flatMap(_.allergens)

    lazy val safeIngredients: Set[Ingredient] = allIngredients -- allergenMap.values.toSet
    lazy val dangerousIngredients: Seq[Ingredient] = allergenMap.toList.sortBy(_._1.value).map(_._2)

    lazy val allergenMap: Map[Allergen, Ingredient] = {
      def allergenToCandidateIngredients(targetAllergen: Allergen): (Allergen, Set[Ingredient]) =
        targetAllergen -> foods
          .collect { case Food(ingredients, allergens) if allergens contains targetAllergen => ingredients }
          .reduce(_ intersect _)

      @tailrec
      def filter(ingMap: Map[Allergen, Set[Ingredient]], matched: Map[Allergen, Ingredient]): Map[Allergen, Ingredient] = {
        if (ingMap.isEmpty) matched
        else {
          val Some((allergen, List(ingredient))) = ingMap.view.mapValues(_.toList).find(_._2.size == 1)
          val updatedMatched = matched + (allergen -> ingredient)
          val updatedMap = (ingMap - allergen).view.mapValues(_ - ingredient).toMap
          filter(updatedMap, updatedMatched)
        }
      }

      filter(allAllergens.map(allergenToCandidateIngredients).toMap, Map.empty)
    }
  }

  def parse(input: List[String]): Set[Food] = {
    input.map { line =>
      val Array(ingredients, allergens) = line.split(" \\(contains ")
      val ingsParsed = ingredients.split(" ").toSet.map(Ingredient)
      val allsParsed = allergens.dropRight(1).split(", ").toSet.map(Allergen)
      Food(ingsParsed, allsParsed)
    }.toSet
  }

  def main(args: Array[String]): Unit = {
    val puzzle = AllergensPuzzle(parse(inputLines))

    println("Step 1: " + puzzle.safeIngredients.map(si => puzzle.ingredientsSets.count(_.contains(si))).sum) //2262
    println("Step 2: " + puzzle.dangerousIngredients.map(_.value).mkString(","))

  }

}

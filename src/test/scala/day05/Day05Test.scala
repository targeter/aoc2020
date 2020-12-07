package day05

import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Day05Test extends AnyFunSuite with Matchers with OptionValues with TableDrivenPropertyChecks {
  import Day05._

  test("test") {
    val examples = Table(
      ("pass", "row", "col", "id"),
      ("BFFFBBFRRR", 70, 7, 567),
      ("FFFBBBFRRR", 14, 7, 119),
      ("BBFFBBFRLL", 102, 4, 820)
    )

    forAll(examples) {
      case (pass, row, col, id) =>
        findRow(pass) must be(row)
        findCol(pass) must be(col)
        seatId(pass) must be(id)
    }

  }

}

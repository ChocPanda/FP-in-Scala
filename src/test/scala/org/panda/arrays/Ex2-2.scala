package org.panda.arrays

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.panda.tags._

class ArraySpec extends FreeSpec with Matchers with PropertyChecks {
  "The Sorted object" - {
    "isSorted should return true for a simple sequence" in {
        Sorted.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b) shouldBe true
    }
  }
}
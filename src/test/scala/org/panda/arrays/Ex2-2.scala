package org.panda.arrays

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.panda.tags._

class ArraySpec extends FreeSpec with Matchers {
  "The Sorted object" - {
    "isSorted should return true for a simple sequence of integers" in {
        Sorted.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a <= b) shouldBe true
        Sorted.isSorted(Array(3, 2, 1), (a: Int, b: Int) => a >= b) shouldBe true
    }

    "isSorted should return true for an empty array" in {
        Sorted.isSorted(Array[Int](), (a: Int, b: Int) => a <= b) shouldBe true
    }
  }
}
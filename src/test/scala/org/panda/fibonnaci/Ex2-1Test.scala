package org.panda.fibonacci

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.panda.tags._

class FibonacciSpec extends FreeSpec with Matchers with PropertyChecks {
  "The Fibonacci object" - {
    "fib(1) should return 0" in {
      Fibonacci.fib(1) shouldEqual 0
    }

    "fib(2) should return 1" in {
      Fibonacci.fib(2) shouldEqual 1
    }

    "fib(3) should return 1" in {
      Fibonacci.fib(3) shouldEqual 1
    }

    "fib(4) should return 2" in {
      Fibonacci.fib(4) shouldEqual 2
    }

		"fib" - {
    	"should return the sum of the previous 2 fibonacci numbers when passed a positive argument" taggedAs(Slow) in {
				forAll(Gen.posNum[Int]) { n: Int =>
					whenever(n > 2) {
						Fibonacci.fib(n) shouldEqual Fibonacci.fib(n - 1) + Fibonacci.fib(n - 2)
					}
				}
			}

			"should throw an IllegalArgumentException when passed a negative number" taggedAs(Slow) in {
				forAll(Gen.negNum[Int]) { n: Int =>
					intercept[IllegalArgumentException] { Fibonacci.fib(n) }
				}
			}
		}
  }
}

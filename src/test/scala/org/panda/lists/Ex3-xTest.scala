package org.panda.lists

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.panda.tags._

class ListsTests extends FreeSpec with Matchers with PropertyChecks {
    "Lists.tail should" - {
        "match the behaviour of the scala.collections.List.tail" taggedAs(Slow) in {
            forAll (Gen.nonEmptyListOf(Gen.posNum[Int])) { list => 
                Lists.tail(list) shouldEqual list.tail
            }
        }

        "return Nil when passed an empty list" in {
            Lists.tail(Nil) shouldBe Nil
        }
    }

    "Lists.drop should" - {
        "match the behaviour of the scala.collections.List.drop" taggedAs(Slow) in {
            forAll (Gen.nonEmptyListOf(Gen.posNum[Int]), Gen.posNum[Int]) { (list, n) => 
                Lists.drop(n, list) shouldEqual list.drop(n)
            }
        }

        "return Nil when passed an empty list" in {
            forAll { n: Int =>
                Lists.drop(n, Nil) shouldBe Nil
            }
        }
    }

    "Lists.init should" - {
        "match the behaviour of the scala.collections.List.dropRight" in {
            forAll { list: List[Int] =>
                whenever(list.size > 2) {
                    Lists.init(list) shouldEqual list.dropRight(1)
                }
            }
        }

        val nilCases = {
            for { 
                x       <- Gen.posNum[Int]
                list    <- Gen.oneOf(Nil, List(x))
            } yield list
        }

        "return Nil when passed empty lists and lists with only a single element" in {
            forAll (nilCases) { list: List[Int] => Lists.init(list) shouldBe Nil }
        }
    }
}

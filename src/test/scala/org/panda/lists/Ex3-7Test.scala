package org.panda.lists

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class MyListTests extends FreeSpec with Matchers with PropertyChecks {
    implicit def myListGen[A](implicit aGen: Gen[A], lGen: Gen[MyList[A]]) = for {
        head <- aGen
        tail <- lGen
        myList <- Gen.oneOf(MyNil, Cons(head, MyNil), Cons(head, tail))
    } yield myList

    "MyList.sum should" - {
        "match the sum of a scala.collections.List[Int]" in {
            forAll { list: List[Int] =>
                val myList = MyList.apply(list: _*)
                myList.sum shouldEqual list.sum
            }
        }

        "return the product of a scala.collections.List[Int]" in {
            forAll { list: List[Int] =>
                val myList = MyList.apply(list: _*)
                myList.product shouldEqual list.fold(1)(_ * _)
            }
        }
    }
}
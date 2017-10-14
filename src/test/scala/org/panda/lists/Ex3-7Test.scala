package org.panda.lists

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class MyListTests extends FreeSpec with Matchers with PropertyChecks {

    /**
    This is unused I just wanted to try and write it. It could be improved by increasing the probablity
    of having longer lists or by better integrating my list with the scala.Collections library by implementing
    TraversableLike, etc... so scalacheck could simply use Gen.containerOf, however that felt wildly out of scope.
     **/     
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

    "MyList.product should" - {
        "return the product of a scala.collections.List[Int]" in {
            forAll { list: List[Int] =>
                val myList = MyList.apply(list: _*)
                myList.product shouldEqual list.fold(1)(_ * _)
            }
        }
    }
}
package org.panda.lists

import org.scalatest._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class MyListTests extends FlatSpec with Matchers with PropertyChecks {

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

    "MyList.sum && MyList.lsum" should "match the sum of a scala.collections.List[Int]" in {
        forAll { list: List[Int] =>
            val myList = MyList.apply(list: _*)
            val listSum = list.sum

            myList.sum shouldEqual listSum
            myList.lSum shouldEqual listSum
        }
    }

    "MyList.product && MyList.lProduct" should "return the product of a scala.collections.List[Int]" in {
        forAll { list: List[Int] =>
            val myList = MyList.apply(list: _*)
            val listProd = list.fold(1)(_ * _)

            myList.product shouldEqual listProd
            myList.lProduct shouldEqual listProd
        }
    }

    "MyList.length && MyList.lLength" should "return the same length as a scala.collections.List[Int]" in {
        forAll { list: List[Int] =>
            val myList = MyList.apply(list: _*)
            val length = list.length

            myList.length shouldEqual length
            myList.lLength shouldEqual length
        }        
    }

    // "MyList.reverse" should "return the same list as a scala.collections.List[Int]" in {
    //     forAll { list: List[Int] =>
    //         val myList = MyList.apply(list: _*)

    //         myList.reverse shouldEqual
    //     }
    // }

    "MyList.reverse" should "return MyList(3, 2, 1) from MyList(1, 2, 3)" in {
        MyList(1, 2, 3).reverse shouldEqual MyList(3, 2, 1)
    }

    "test" should "demonstrate Exercise 3.8" in {
        println(MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
    }
}
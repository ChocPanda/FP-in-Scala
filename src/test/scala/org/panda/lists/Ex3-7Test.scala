package org.panda.lists

import org.scalatest._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Gen._
import org.scalatest.prop.PropertyChecks

class MyListTests extends FlatSpec with Matchers with PropertyChecks {

    implicit def myListGen[T](implicit aGen: Arbitrary[T]): Arbitrary[MyList[T]] = Arbitrary {
        for {
            seq <- Gen.containerOf[Seq, T](aGen.arbitrary)
        } yield MyList.apply(seq: _*)
    }

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

    "MyList.reverse" should "return the same list as a scala.collections.List[Int]" in {
        forAll { list: List[Int] =>
            val myList = MyList.apply(list: _*)

            myList.reverse shouldEqual MyList.apply(list.reverse: _*)
        }
    }

    it should "be it's own inverse function" in {
        forAll { list: MyList[Int] =>
            list.reverse.reverse shouldEqual list
        }
    }

    "MyList.reverse" should "return MyList(3, 2, 1) from MyList(1, 2, 3)" in {
        MyList(1, 2, 3).reverse shouldEqual MyList(3, 2, 1)
    }

    "MyList.append" should "append an item to the end of the list" in {
        MyList(1, 2, 3).append(4) shouldEqual MyList(1, 2, 3, 4)
    }

    "MyList.flatten" should "flatten a list of lists of A to a list of A's" in {
        MyList.flatten(MyList(MyList(1, 2, 3), MyList(4, 5, 6))) shouldEqual MyList(1, 2, 3, 4, 5, 6)
    }

    it should "match the behaviour of a scala.collections.list.flatten" in {
        forAll { list: List[List[Int]] => 
            val myList = MyList(list.map(MyList(_: _*)): _*)

            MyList.flatten(myList) shouldEqual MyList(list.flatten: _*)
        }
    }

    "MyList.mapPlus1" should "match the behaviour of scala.collections.List[Int].map (_ + 1)" in {
        forAll { list: List[Int] =>
            val myList = MyList(list: _*)

            myList.mapPlus1 shouldEqual MyList(list.map(_ + 1): _*)
        }
    }

    "MyList[Double].map(_.toString)" should "match the behaviour of scala.collections.List[Double].map(_.toString)" in {
        forAll { list: List[Double] =>
            val myList = MyList(list: _*)

            myList.map(_.toString) shouldEqual MyList(list.map(_.toString): _*)
        }
    }

    "MyList.filter(_ % 2 == 0)" should "match the behaviour of scala.collections.List.filter(_ % 2 == 0)" in {
        forAll { list: List[Int] =>
            val myList = MyList(list: _*)

            myList.filter(_ % 2 == 0) shouldEqual MyList(list.filter(_ % 2 == 0): _*)
        }
    }

    "MyList.flatMap(i => MyList(i, i))" should "match the behaviour of scala.collections.List.flatMap(i => List(i, i))" in {
        forAll { list: List[Int] =>
            val myList = MyList(list: _*)

            myList.flatMap(i => MyList(i, i)) shouldEqual MyList(list.flatMap(i => List(i, i)): _*)
        }
    }

    "MyList.flatMapFilter(_ % 2 == 0)" should "match the behaviour of MyList.flatMap(_ % 2 == 0)" in {
        forAll { myList: MyList[Int] => myList.flatMapFilter(_ % 2 == 0) shouldEqual myList.filter(_ % 2 == 0) }
    }

    "MyList.sumValues" should "match the behaviour of scala.collections.List.zipped.map(_ + _)" in {
        forAll { (list1: List[Int], list2: List[Int]) =>
            val (myList1, myList2) = (MyList(list1: _*), MyList(list2: _*))
            MyList.sumValues(myList1, myList2) shouldEqual MyList((list1, list2).zipped.map(_ + _): _*)
        }
    }

    "MyList.zipWith(...)(_ + _)" should "match the behaviour of MyList.sumValues" in {
        forAll { (myList1: MyList[Int], myList2: MyList[Int]) =>
            MyList.zipWith(myList1, myList2)(_ + _) shouldEqual MyList.sumValues(myList1, myList2)
        }
    }

    "test" should "demonstrate Exercise 3.13" in {
        println("---------------------------------------------------------------------------------------------------------------------------")
        println("Exercise 3.8: " + MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
        println("---------------------------------------------------------------------------------------------------------------------------")
        println("Exercise 3.13: foldLeftFromFoldRight - " + MyList.foldLeftFromFoldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
        println("Exercise 3.13: foldLeft              - " + MyList.foldLeft(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
        println("---------------------------------------------------------------------------------------------------------------------------")
        println("Exercise 3.13: foldRightFromFoldLeft - " + MyList.foldRightFromFoldLeft(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
        println("Exercise 3.13: foldRight             - " + MyList.foldRight(MyList(1, 2, 3), MyNil: MyList[Int])(Cons(_, _)))
    }
}
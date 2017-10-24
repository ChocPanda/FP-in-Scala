package org.panda.lists

import scala.annotation.tailrec

sealed trait MyList[+A] {
    def foldRight[P >: A, Q](as: MyList[P], base: Q)(f: (P, Q) => Q): Q = MyList.foldRight(this, base)(f)

    def foldRight[P >: A, Q](base: Q, shortCircuitCase: P => Boolean, shortCircuit: Q)(f: (P, Q) => Q): Q = {
        MyList.foldRight(this, base, shortCircuitCase, shortCircuit)(f)
    }
    
    def sum[B >: A](implicit num: Numeric[B]): B = MyList.sum(num)(this)
    def lSum[B >: A](implicit num: Numeric[B]): B = MyList.lSum(num)(this)

    def product[B >: A](implicit num: Numeric[B]): B = MyList.product(num)(this)
    def lProduct[B >: A](implicit num: Numeric[B]): B = MyList.lProduct(num)(this)

    def length: Int = MyList.length(this)
    def lLength: Int = MyList.lLength(this)

    def reverse: MyList[A] = MyList.reverse(this)

    def append[U >: A]: U => MyList[U] = MyList.append(this, _)

    def mapPlus1[B >: A](implicit num: Numeric[B]): MyList[B] = MyList.mapPlus1(num)(this)

    def map[B](f: A => B): MyList[B] = MyList.map(f)(this)

    def flatMap[B](f: A => MyList[B]): MyList[B] = MyList.flatMap(f)(this)

    def filter(predicate: A => Boolean): MyList[A] = MyList.filter(predicate)(this)

    def flatMapFilter(predicate: A => Boolean): MyList[A] = MyList.flatMapFilter(predicate)(this)
}

case object MyNil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
    def foldRight[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = foldRight(as, base, (_: A) => false, base)(f)

    def foldRight[A, B](as: MyList[A], base: B, shortCircuitCase: A => Boolean, shortCircuit: B)(f: (A, B) => B): B = {
        as match {
            case MyNil => base
            case Cons(x, _) if shortCircuitCase(x) => shortCircuit
            case Cons(x, xs) => f(x, foldRight(xs, base)(f))
        }
    }

    def sum[A](implicit num: Numeric[A]): MyList[A] => A = foldRight(_, num.zero)(num.plus)

    // Exercise 3.7
    def product[A](implicit num: Numeric[A]): MyList[A] => A = foldRight(_, num.one, (x: A) => x == num.zero, num.zero)(num.times)

    def apply[A](as: A*): MyList[A] = {
        if (as.isEmpty) MyNil
        else Cons(as.head, apply(as.tail: _*))
    }

    // Exercise 3.9
    def length[A]: MyList[A] => Int = foldRight(_, 0)( (_, len) => len + 1)
    
    // Exercise 3.10
    def foldLeft[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = foldLeft(as, base, (_: A) => false, base)(f)

    def foldLeft[A, B](as: MyList[A], base: B, shortCircuitCase: A => Boolean, shortCircuit: B)(f: (A, B) => B): B = {
        @tailrec
        def loop(l: MyList[A], acc: B): B = l match {
            case MyNil => acc
            case Cons(x, _) if shortCircuitCase(x) => shortCircuit
            case Cons(x, xs) => loop(xs, f(x, acc))
        }
        
        loop(as, base)
    }

    // Exercise 3.11
    def lSum[A](implicit num: Numeric[A]): MyList[A] => A = foldLeft(_, num.zero)(num.plus)

    def lProduct[A](implicit num: Numeric[A]): MyList[A] => A = foldLeft(_, num.one, (x: A) => x == num.zero, num.zero)(num.times)

    def lLength[A]: MyList[A] => Int = foldLeft(_, 0)( (_, len) => len + 1)

    // Exercise 3.12
    def reverse[A]: MyList[A] => MyList[A] = foldLeft(_, MyNil: MyList[A])(Cons(_, _))

    // Exercise 3.13
    def foldLeftFromFoldRight[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = 
        foldRight(as, (b: B) => b)((a, g) => g compose (b => f(a, b)))(base)
    
    def foldRightFromFoldLeft[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = 
        foldLeft(as, (b: B) => b)((a, g) => g compose (b => f(a, b)))(base)

    // Exercise 3.14
    def append[A, U >: A]:(MyList[A], U) => MyList[U] = (as, u) => foldRight(as, MyList(u))(Cons(_, _))

    // Exercise 3.15
    def flatten[A]:(MyList[MyList[A]]) => MyList[A] = as => foldRight(as, MyNil: MyList[A])((bs, ns) => foldRight(bs, ns)(Cons(_, _)))

    // Exercise 3.16
    def mapPlus1[A](implicit num: Numeric[A]): MyList[A] => MyList[A] = foldRight(_, MyNil: MyList[A])((a, res) => Cons(num.plus(a, num.one), res))

    // I implemented 3.18 naturally to solve 3.17 before reading that it was an exercise... That's why it's not here... Promise

    // Exercise 3.18
    def map[A, B](f: A => B): MyList[A] => MyList[B] = foldRight(_, MyNil: MyList[B])((a, res) => Cons(f(a), res))

    // Exercise 3.19
    def filter[A](predicate: A => Boolean): MyList[A] => MyList[A] = foldRight(_, MyNil: MyList[A])((a, res) => if (predicate(a)) Cons(a, res) else res)

    // Exercise 3.20
    def flatMap[A, B](f: A => MyList[B]): MyList[A] => MyList[B] = foldRight(_, MyNil: MyList[B])((a, res) => foldRight(f(a), res)(Cons(_, _)))

    // Exercise 3.21
    def flatMapFilter[A](predicate: A => Boolean): MyList[A] => MyList[A] = flatMap(a => if (predicate(a)) Cons(a, MyNil) else MyNil)

    // Exercise 3.22
    def sumValues[A](l1: MyList[A], l2: MyList[A])(implicit num: Numeric[A]): MyList[A] = (l1, l2) match {
        case (Cons(a, as), Cons(b, bs)) => Cons(num.plus(a, b), sumValues(as, bs))
        case _                          => MyNil
    }

    // Exercise 3.23
    def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] = (l1, l2) match {
        case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
        case _                          => MyNil
    }
}

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
}

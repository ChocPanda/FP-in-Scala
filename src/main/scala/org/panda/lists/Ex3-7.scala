package org.panda.lists

sealed trait MyList[+A] {
    // def foldRight[A, B](as: MyList[A], base: B)(f: (A, B) => B): B = MyList.foldRight(this, base)(f)

    // def foldRight[A, B](base: B, shortCircuit: A => Boolean)(f: (A, B) => B): B = MyList.foldRight(this, base, shortCircuit)(f)
    
    def sum[B >: A](implicit num: Numeric[B]): B = MyList.sum(num)(this)

    def product[B >: A](implicit num: Numeric[B]): B = MyList.product(num)(this)
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

    def product[A](implicit num: Numeric[A]): MyList[A] => A = foldRight(_, num.one, (x: A) => x == num.zero, num.zero)(num.times)

    def apply[A](as: A*): MyList[A] = {
        if (as.isEmpty) MyNil
        else Cons(as.head, apply(as.tail: _*))
    }
}

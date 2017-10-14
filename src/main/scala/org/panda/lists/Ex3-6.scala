package org.panda.lists

/**
    The answers to exercises 3.2 to 3.6, I completed them using the scala.collections.List
**/

object Lists extends App {
    // Exercise 3.2
    def tail[A]: List[A] => List[A] = _ match {
        case x :: xs => xs
        case Nil => Nil
    }

    // Exercise 3.3
    def setHead[A]: (A, List[A]) => List[A] = (a, l) => a :: l

    // Exercise 3.4
    def drop[A]: (Int, List[A]) => List[A] = (n, l) => (n, l) match {
        case (_, Nil)       => Nil
        case (0, list)      => list
        case (n, x :: xs)   => drop(n - 1, xs)
    }

    // Exercise 3.5
    def dropWhile[A]: (List[A], A => Boolean) => List[A] = (l, f) => l match {
        case Nil                => Nil
        case x :: xs if f(x)    => dropWhile(xs, f)
        case list               => list
    }

    // Exercise 3.6
    def init[A]: List[A] => List[A] = _ match {
        case Nil            => Nil
        case x :: Nil       => Nil
        case x :: xs        => x :: init(xs)
    }
}
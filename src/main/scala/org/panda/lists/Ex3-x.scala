package org.panda.lists

object Lists extends App {
    def tail[A]: List[A] => List[A] = _ match {
        case x :: xs => xs
        case Nil => Nil
    }

    def drop[A]: (Int, List[A]) => List[A] = (n, l) => (n, l) match {
        case (_, Nil)       => Nil
        case (0, list)      => list
        case (n, x :: xs)   => drop(n - 1, xs)
    }

    def dropWhile[A]: (List[A], A => Boolean) => List[A] = (l, f) => l match {
        case Nil                => Nil
        case x :: xs if f(x)    => dropWhile(xs, f)
        case list               => list
    }

    def setHead[A]: (A, List[A]) => List[A] = (a, l) => a :: l

    def init[A]: List[A] => List[A] = _ match {
        case Nil            => Nil
        case x :: Nil       => Nil
        case x :: xs        => x :: init(xs)
    }
}
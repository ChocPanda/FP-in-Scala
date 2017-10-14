package org.panda.curry

 object Uncurry extends App {
     def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)
 }
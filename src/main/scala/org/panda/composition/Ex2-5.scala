package org.panda.composition

 object Composition extends App {
     def compose[A, B, C](f: B => C, g: A => B): A => C = g andThen f

     def firstPrinciplesCompose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
 }
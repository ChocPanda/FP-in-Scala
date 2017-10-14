package org.panda.composition

 object Composition extends App {
     def compose[A, B, C](f: B => C, g: A => B): A => C = g andThen f
 }
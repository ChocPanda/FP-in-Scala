package org.panda.fibonacci

import scala.annotation.tailrec

 object Fibonacci extends App {
    def fib(n: Int): Int = {
        @tailrec       
        def go(n: Int, curr: Int, acc: Int): Int = {
            require(n > 0, s"The value n: $n should not be less than 1")

            if (n == 1) acc
            else go (n-1, acc, curr + acc)
        }

        go(n, 1, 0)
    }
}
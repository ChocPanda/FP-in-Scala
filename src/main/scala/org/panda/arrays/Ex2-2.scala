package org.panda.arrays

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Sorted {
    @tailrec
    def isSorted[A: ClassTag](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        as match {
            case Array(a, b, tail@_*) if ordered(a, b) => isSorted(tail.toArray, ordered)
            case Array(_, _, _*) => false
            case _ => true 
        }
    }
}
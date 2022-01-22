package chapter2

import scala.annotation.tailrec

object PolymorphicFunc extends App {

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean ={

    @tailrec
    def loop(n: Int): Boolean ={
      if(n >= as.length - 1) true
      else
        if (!ordered(as(n),as(n + 1))) false
        else loop(n + 1)
    }
      loop(0)
  }
  def order(x: Int, y: Int): Boolean = {
    if(x < y) true
    else false
  }
  println(isSorted(Array(1,2,3,4,0),order))
}

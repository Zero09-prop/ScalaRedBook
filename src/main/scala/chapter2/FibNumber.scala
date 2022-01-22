package chapter2

import scala.annotation.tailrec

object FibNumber extends App {
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, a: Int, b: Int): Int = {
      if(n <= 1) a
      else
        go(n-1,b,a + b)
    }

    go(n,0,1)
  }
  println(fib(1))
}

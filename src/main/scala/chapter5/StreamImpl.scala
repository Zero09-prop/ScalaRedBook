package chapter5

import chapter5.StreamImpl.MyStream.{cons, empty}

object StreamImpl extends App {
  trait MyStream[+A] {
    def uncons: Option[(A, MyStream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] =
      uncons match {
        case Some(value) => List(value._1) ::: value._2.toList
        case None        => List()
      }
    def take(n: Int): MyStream[A] =
      uncons match {
        case Some(value) => if (n > 0) cons(value._1, value._2.take(n - 1)) else empty
        case None        => empty
      }

    def takeWhile(p: A => Boolean): MyStream[A] =
      uncons match {
        case Some(value) => if (p(value._1)) cons(value._1, value._2.takeWhile(p)) else empty
        case None        => empty
      }

  }
  object MyStream {
    def empty[A]: MyStream[A] = new MyStream[A] { def uncons: Option[(A, MyStream[A])] = None }
    def cons[A](hd: => A, tl: => MyStream[A]): MyStream[A] =
      new MyStream[A] {
        lazy val uncons: Some[(A, MyStream[A])] = Some((hd, tl))
      }

    def apply[A](as: A*): MyStream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }

  println(MyStream(1, 3, 3, 4, 5).take(3).toList)
}

package chapter5

import chapter5.StreamImpl.MyStream.{cons, empty}

object StreamImpl extends App {
  trait MyStream[+A] {
    def uncons: Option[(A, MyStream[A])]
    def isEmpty: Boolean = uncons.isEmpty
    def toList: List[A] =
      uncons match {
        case Some((h, t)) => List(h) ::: t.toList
        case None         => List()
      }
    def take(n: Int): MyStream[A] =
      uncons match {
        case Some((h, t)) => if (n > 0) cons(h, t.take(n - 1)) else empty
        case None         => empty
      }

    def takeWhile(p: A => Boolean): MyStream[A] =
      uncons match {
        case Some((h, t)) => if (p(h)) cons(h, t.takeWhile(p)) else empty
        case None         => empty
      }
    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      uncons match {
        case Some((h, t)) => f(h, t.foldRight(z)(f))
        case None         => z
      }

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhile2(p: A => Boolean): MyStream[A] =
      foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

    def map[B](f: A => B): MyStream[B] =
      foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(p: A => Boolean): MyStream[A] =
      foldRight(empty[A])((el, str) => if (p(el)) cons(el, str) else str)

    def append[B >: A](s: => MyStream[B]): MyStream[B] =
      foldRight(s)((el1, str1) => cons(el1, str1))

    def flatMap[B](f: A => MyStream[B]): MyStream[B] =
      foldRight(empty[B])((el, str) => f(el) append (str))
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
  val ones: MyStream[Int] = cons(1, ones)
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1))
  println(ones.forAll(_ != 1))

}

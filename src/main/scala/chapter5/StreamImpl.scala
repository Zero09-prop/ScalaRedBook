package chapter5

object StreamImpl extends App {
  import MyStream._
  trait MyStream[+A] {
    import MyStream._
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
      foldRight(empty[B])((el, str) => f(el) append str)

    def mapUnfold[B](f: A => B): MyStream[B] =
      unfold(this)(s =>
        s.uncons match {
          case Some((h, t)) => Some((f(h), t))
          case None         => None
        }
      )

    def takeUnfold(n: Int): MyStream[A] =
      unfold((this, n))(s =>
        if (s._2 > 0) s._1.uncons match {
          case Some((h, t)) => Some((h, (t, s._2 - 1)))
          case None         => None
        }
        else None
      )

    def takeWhileUnfold(p: A => Boolean): MyStream[A] =
      unfold(this)(s =>
        s.uncons match {
          case Some((h, t)) => if (p(h)) Some((h, t)) else None
          case None         => None
        }
      )

    def zip[B](str: MyStream[B]): MyStream[(A, B)] =
      unfold((this, str))(s =>
        (s._1.uncons, s._2.uncons) match {
          case (Some((h1, t1)), Some((h2, t2))) => Some(((h1, h2), (t1, t2)))
          case _                                => None
        }
      )
    def zipAll[B](sb: MyStream[B]): MyStream[(Option[A], Option[B])] =
      unfold((this, sb))(s =>
        (s._1.uncons, s._2.uncons) match {
          case (Some((h1, t1)), Some((h2, t2))) => Some(((Some(h1), Some(h2)), (t1, t2)))
          case (Some((h1, t1)), None)           => Some(((Some(h1), None), (t1, empty[B])))
          case (None, Some((h2, t2)))           => Some(((None, Some(h2)), (empty[A], t2)))
          case _                                => None
        }
      )

    def startsWith[AA >: A](s2: MyStream[AA]): Boolean = this.zip(s2).forAll(x => x._1 == x._2)

    def tails: MyStream[MyStream[A]] =
      unfold((this, empty[A]))(s =>
        s._1.uncons match {
          case Some((_, t)) => Some((s._2.append(s._1), (t, s._2)))
          case None         => None
        }
      )

    def scanRight[B](z: B)(f: (A, => B) => B): MyStream[B] = this.tails.map(s => s.foldRight(z)(f))
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

    def constant[A](a: A): MyStream[A] = cons(a, constant(a))

    def from(n: Int): MyStream[Int] = cons(n, from(n + 1))

    def fibs: MyStream[Long] = {
      def go(a: => Long, b: => Long): MyStream[Long] = cons(a, go(b, a + b))
      go(0, 1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = {
      f(z) match {
        case Some((h, t)) => cons(h, unfold(t)(f))
        case None         => empty
      }
    }
    def constantUnfold[A](a: A): MyStream[A] = unfold(a)(s => Option((s, s)))
    def fromUnfold(n: Int): MyStream[Int] = unfold(n)(s => Option((s, s + 1)))
    def onesUnfold: MyStream[Int] = unfold(1)(_ => Option((1, 1)))
    def fibsUnfold: MyStream[Long] = unfold((0, 1))(s => Option((s._1, (s._2, s._1 + s._2))))
  }
  val ones: MyStream[Int] = cons(1, ones)
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(MyStream(1, 1, 1, 1, 2, 3).takeWhileUnfold(_ == 1).toList)
  println(ones.forAll(_ != 1))
  println(onesUnfold.take(5).toList)
  println(constantUnfold(1).take(5).toList)
  println(fromUnfold(1).takeUnfold(10).toList)
  println(fibsUnfold.take(5).toList)
  println(MyStream(1, 2, 3).mapUnfold(_ * 2).toList)
  println(MyStream(1, 3, 5).zip(MyStream(2, 4, 6)).toList)
  println(MyStream(1, 3, 5).zipAll(MyStream(2, 4)).toList)
  println(MyStream(1, 2, 3).startsWith(MyStream(1, 2)))
  println(MyStream(1, 2, 3, 4, 5).tails.toList.map(_.toList.length))
  println(MyStream(1, 2, 3).scanRight(1)(_ + _).toList)
}

package chapter2

object ListImpl extends App {
  sealed trait MyList[+A]

  case object Nil extends MyList[Nothing]

  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def sum(ints: MyList[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: MyList[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): MyList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](ls: MyList[A]): MyList[A] = ls match {
      case Nil => Nil
      case Cons(_, al) => al
    }

    def setHead[A](ls: MyList[A], el: A): MyList[A] = ls match {
      case Nil => Cons(el, Nil)
      case t: Cons[A] => Cons(el, t)
    }
  }

  val x = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + MyList.sum(t)
    case _ => 101
  }

}

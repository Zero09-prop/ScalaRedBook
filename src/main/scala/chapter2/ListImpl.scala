package chapter2

import scala.annotation.tailrec

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

    @tailrec
    def drop[A](ls: MyList[A], n: Int): MyList[A] = n match {
      case 1 => tail(ls)
      case _ => drop(tail(ls), n - 1)
    }

    @tailrec
    def dropWhile[A](ls: MyList[A], f: A => Boolean): MyList[A] = ls match {
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
      case _ => ls
    }

    def init[A](ls: MyList[A]): MyList[A] = ls match {
      case Cons(h, t) => if (t != Nil) Cons(h, init(t)) else Nil
    }

    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](as: MyList[A]): Int = foldRight(as, 0)((_, b) => b + 1)

    @tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(h, _) => foldLeft(tail(as), f(z, h))(f)
    }

    def sum2(ls: MyList[Int]): Int = foldLeft(ls, 0)(_ + _)

    def product2(ls: MyList[Double]): Double = foldLeft(ls, 1.0)(_ * _)

    def length2[A](ls: MyList[A]): Int = foldLeft(ls, 0)((z, _) => z + 1)
  }

  val x = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + MyList.sum(t)
    case _ => 101
  }
  println(MyList.length2(MyList(1, 2, 5)))
}

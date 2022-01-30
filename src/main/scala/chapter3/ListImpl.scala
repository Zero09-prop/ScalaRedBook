package chapter3

import scala.annotation.tailrec

object ListImpl extends App {
  sealed trait MyList[+A]

  case object Nil extends MyList[Nothing]

  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def sum(ints: MyList[Int]): Int =
      ints match {
        case Nil         => 0
        case Cons(x, xs) => x + sum(xs)
      }

    def product(ds: MyList[Double]): Double =
      ds match {
        case Nil         => 1.0
        case Cons(x, xs) => x * product(xs)
      }

    def apply[A](as: A*): MyList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](ls: MyList[A]): MyList[A] =
      ls match {
        case Nil         => Nil
        case Cons(_, al) => al
      }

    def setHead[A](ls: MyList[A], el: A): MyList[A] =
      ls match {
        case Nil        => Cons(el, Nil)
        case t: Cons[A] => Cons(el, t)
      }

    @tailrec
    def drop[A](ls: MyList[A], n: Int): MyList[A] =
      n match {
        case 1 => tail(ls)
        case _ => drop(tail(ls), n - 1)
      }

    @tailrec
    def dropWhile[A](ls: MyList[A], f: A => Boolean): MyList[A] =
      ls match {
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else Cons(h, t)
        case _          => ls
      }

    def init[A](ls: MyList[A]): MyList[A] =
      ls match {
        case Cons(h, t) => if (t != Nil) Cons(h, init(t)) else Nil
      }

    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil         => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def length[A](as: MyList[A]): Int = foldRight(as, 0)((_, b) => b + 1)

    @tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil        => z
        case Cons(h, _) => foldLeft(tail(as), f(z, h))(f)
      }

    def sum2(ls: MyList[Int]): Int = foldLeft(ls, 0)(_ + _)

    def product2(ls: MyList[Double]): Double = foldLeft(ls, 1.0)(_ * _)

    def length2[A](ls: MyList[A]): Int = foldLeft(ls, 0)((z, _) => z + 1)

    def reverse[A](ls: MyList[A]): MyList[A] = foldLeft(ls, Nil: MyList[A])((x, y) => Cons(y, x))

    def foldLeft2[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = foldRight(as, z)((a, b) => f(b, a))

    def foldRight2[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

    def prepend[A](ls: MyList[A], a: A): MyList[A] =
      ls match {
        case t: Cons[A] => Cons(a, t)
        case _          => Cons(a, Nil)
      }

    def append1[A](ls1: MyList[A], ls2: MyList[A]): MyList[A] = foldRight(ls1, ls2)((e, l) => Cons(e, l))

    def append2[A](ls1: MyList[A], ls2: MyList[A]): MyList[A] = foldLeft(reverse(ls1), ls2)((l, e) => Cons(e, l))

    def flat[A](lst: MyList[MyList[A]]): MyList[A] =
      lst match {
        case Nil        => Nil
        case Cons(h, t) => append2(foldRight(h, Nil: MyList[A])((el, a) => Cons(el, a)), flat(t))
      }

    def inc(ls: MyList[Int]): MyList[Int] =
      ls match {
        case Nil        => Nil
        case Cons(h, t) => Cons(h + 1, inc(t))
      }

    def lstString(ls: MyList[Double]): MyList[String] =
      ls match {
        case Nil        => Nil
        case Cons(h, t) => Cons(h.toString, lstString(t))
      }

    def map[A, B](ls: MyList[A])(f: A => B): MyList[B] =
      reverse(foldLeft(ls, Nil: MyList[B])((l, el) => Cons(f(el), l)))

    def filter[A](ls: MyList[A])(f: A => Boolean): MyList[A] =
      reverse(foldLeft(ls, Nil: MyList[A])((l, el) => if (f(el)) Cons(el, l) else l))

    def flatMap[A, B](ls: MyList[A])(f: A => MyList[B]): MyList[B] = flat(map(ls)(f))

    def filter2[A](ls: MyList[A])(f: A => Boolean): MyList[A] =
      flatMap(ls) { a =>
        if (f(a)) MyList(a) else Nil
      }

    def addLists(l1: MyList[Int], l2: MyList[Int]): MyList[Int] =
      (l1, l2) match {
        case (_, Nil)                     => Nil
        case (Nil, _)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
      }

    def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(f: (A, B) => C): MyList[C] =
      (l1, l2) match {
        case (_, Nil)                     => Nil
        case (Nil, _)                     => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      }

    def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = {
      @tailrec
      def go(l1: MyList[A], l2: MyList[A]): Boolean =
        (l1, l2) match {
          case (Nil, _)                 => false
          case (_, Nil)                 => true
          case (a: Cons[A], b: Cons[A]) => if (a.head == b.head) loop(a.tail, b.tail) else go(a.tail, b)
        }
      @tailrec
      def loop(l1: MyList[A], l2: MyList[A]): Boolean =
        (l1, l2) match {
          case (_, Nil)                 => true
          case (Nil, c: Cons[A])        => false
          case (a: Cons[A], b: Cons[A]) => if (a.head == b.head) loop(a.tail, b.tail) else false
        }

      go(sup, sub)
    }
  }

  val x = MyList(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + MyList.sum(t)
    case _                                     => 101
  }


}

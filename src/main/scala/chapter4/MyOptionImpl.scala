package chapter4

import chapter3.ListImpl.MyList

object MyOptionImpl extends App {
  sealed trait MyOption[+A] {

    def map[B](f: A => B): MyOption[B] =
      this match {
        case MySome(get) => MySome(f(get))
        case _           => MyNone
      }
    def getOrElse[B >: A](default: => B): B =
      this match {
        case MySome(a) => a
        case _         => default
      }

    def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(MyNone)

    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = if (this eq MyNone) MyNone else ob

    def filter(f: A => Boolean): MyOption[A] = flatMap(a => if (f(a)) this else MyNone)

  }
  object MyOption {
    def apply[A](a: A): MyOption[A] = {
      if (a != null) MySome(a)
      else MyNone
    }
    def sequence[A](a: MyList[MyOption[A]]): MyOption[MyList[A]] =
      MyList.foldLeft(a, MyOption(MyList[A]())) { (acc, el) => el.flatMap(e => acc.map(ls => MyList.prepend(ls, e))) }

    def traverse[A, B](ls: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] =
      MyList.foldLeft(ls, MyOption(MyList[B]()))((acc, el) =>
        acc.flatMap(lst => f(el).map(b => MyList.prepend(lst, b)))
      )

    def traverse2[A, B](ls: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] = sequence(MyList.map(ls)(f))

  }
  case class MySome[+A](get: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]
  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = ???
  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def map2[A, B, C](aOpt: MyOption[A], bOpt: MyOption[B])(f: (A, B) => C): MyOption[C] =
    aOpt.flatMap(a => bOpt.flatMap(b => MySome(f(a, b))))

}

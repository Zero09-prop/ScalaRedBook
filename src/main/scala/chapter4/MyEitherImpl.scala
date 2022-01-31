package chapter4

object MyEitherImpl extends App {
  sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] =
      this match {
        case Right(a)   => Right(f(a))
        case l: Left[E] => l
      }
    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
      this match {
        case t: Left[E]   => t
        case Right(value) => f(value)
      }

    def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
      this match {
        case _: Left[E] => b
        case _          => this
      }

    def map2[EE >: E, B, C](bb: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
      for {
        a <- this
        b <- bb
      } yield f(a, b)
  }

  case class Left[+E](value: E) extends MyEither[E, Nothing]
  case class Right[+A](value: A) extends MyEither[Nothing, A]

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    es.foldLeft(Right(List()): MyEither[E, List[A]]) { (ac, el) =>
      for {
        a <- el
        ls <- ac
      } yield (ls :+ a)
    }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    as.foldLeft(Right(List()): MyEither[E, List[B]]) { (ac, a) =>
      for {
        b <- f(a)
        ls <- ac
      } yield (ls :+ b)
    }



}

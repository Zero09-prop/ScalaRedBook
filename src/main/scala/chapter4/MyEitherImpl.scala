package chapter4

object MyEitherImpl extends App {
  sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] =
      this match {
        case Right(a)   => Right(f(a))
        case l: Left[E] => l
      }
    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE,B]
  }
  case class Left[+E](value: E) extends MyEither[E, Nothing]
  case class Right[+A](value: A) extends MyEither[Nothing, A]
}

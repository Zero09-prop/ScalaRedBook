package chapter4
object MyOption extends App {
  sealed trait MyOption[+A] {

    def map[B](f: A => B): MyOption[B] =
      this match {
        case MySome(get) => MySome(f(get))
        case _         => MyNone
      }
    def getOrElse[B >: A](default: => B): B =
      this match {
        case MySome(a) => a
        case _       => default
      }

    def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(MyNone)

    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = if (this eq MyNone) MyNone else ob

    def filter(f: A => Boolean): MyOption[A] = flatMap(a => if (f(a)) this else MyNone)
  }
  case class MySome[+A](get: A) extends MyOption[A]
  case object MyNone extends MyOption[Nothing]


}

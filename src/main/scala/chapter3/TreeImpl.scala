package chapter3

object TreeImpl extends App {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int =
      tree match {
        case _: Leaf[A]   => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }

    def maximum(tree: Tree[Int]): Int =
      tree match {
        case Branch(l, r) => maximum(l) max maximum(r)
        case Leaf(value)  => value
      }

    def depth[A](tree: Tree[A]): Int =
      tree match {
        case Branch(left, right) => 1 + depth(left) max 1 + depth(right)
        case _: Leaf[A]          => 0
      }
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(v)      => Leaf(f(v))
      }
    def fold[A, B](tree: Tree[A], f: A => B)(g: (B, B) => B): B =
      tree match {
        case Leaf(v)      => f(v)
        case Branch(l, r) => g(fold(l, f)(g), fold(r, f)(g))
      }

    def size2[A](tree: Tree[A]): Int =
      fold[A, Int](tree, _ => 1)(1 + _ + _)

    def maximum2(tree: Tree[Int]): Int =
      fold[Int, Int](tree, x => x)(_ max _)

    def depth2[A](tree: Tree[A]): Int =
      fold[A, Int](tree, _ => 0)(1 + _ max 1 + _)

    def map2[A, B](tree: Tree[A])( f: A => B): Tree[B] =
      fold[A, Tree[B]](tree, x => Leaf(f(x)))(Branch(_, _))
  }

  println(Tree.map2(Branch(Leaf(2), Branch(Branch(Leaf(3), Leaf(5)), Leaf(4))))(_ * 2))
}

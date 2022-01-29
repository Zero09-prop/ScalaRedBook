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
        case Branch(l,r) => maximum(l) max maximum(r)
        case Leaf(value) => value
      }

    def depth[A](tree: Tree[A]): Int =
      tree match {
        case Branch(left, right) => 1 + depth(left) max 1 + depth(right)
        case _: Leaf[A] => 0
    }

  }

  println(Tree.depth(Branch(Leaf(2), Branch(Branch(Leaf(3),Leaf(5)), Leaf(4)))))
}

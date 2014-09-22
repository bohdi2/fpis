package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](a: Tree[A]): Int = a match {
    case Leaf(_) => 1
    case Branch(l,r) => size(l) + size(r) + 1

  }

  def maximum(a: Tree[Int]): Int = a match {
    case Leaf(value) => value
    case Branch(l,r) => maximum(l).max(maximum(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](as: Tree[A])(f: A => B, g: (B,B) => B): B = as match {
    case Leaf(v) => f(v)
    case Branch(l,r) => g(fold(l)(f,g), fold(r)(f,g))
  }
}
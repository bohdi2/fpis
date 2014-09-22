package chapter3

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec
  extends FlatSpec
  with Matchers {

  behavior of "Chapter3 Tree"

  "Tree.size" should "find the size of a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    5 should equal(Tree.size(t))
  }

  "Tree.size" should "find the size of an single leaf" in {
    val t = Leaf(1)
    1 should equal(Tree.size(t))
  }

  "Tree.max" should "find the max of a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(21), Leaf(3)))
    21 should equal(Tree.maximum(t))
  }

  "Tree.depth" should "find the depth of a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(21), Leaf(3)))
    3 should equal(Tree.depth(t))
  }

  "Tree.depth" should "find the depth of a leaf" in {
    val t = Leaf(1)
    1 should equal(Tree.depth(t))
  }

  "Tree.map" should "create a new tree" in {
    val t:Tree[Int] = Branch(Leaf(1), Branch(Leaf(21), Leaf(3)))
    val expected = Branch(Leaf(2), Branch(Leaf(42), Leaf(6)))

    expected should equal(Tree.map(t)(v => v + v))
  }

  "Tree.fold size" should "find the size of a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val actual = Tree.fold[Int, Int](t)(leaf => 1, (l,r) => l + r + 1)

    5 should equal(actual)
  }

  "Tree.fold max" should "find the max element in a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(21), Leaf(3)))
    val actual = Tree.fold[Int, Int](t)(leaf => leaf, (l,r) => l max r)

    21 should equal(actual)
  }

  "Tree.fold depth" should "find the depth of a tree" in {
    val t = Branch(Leaf(1), Branch(Leaf(21), Leaf(3)))
    val actual = Tree.fold[Int, Int](t)(leaf => 1, (l,r) => 1 + (l max r))

    3 should equal(actual)
  }

  "Tree.fold map" should "create a copy of the tree" in {
    val t = Branch(Leaf(11), Branch(Leaf(21), Leaf(3)))
    def double(n: Int) = n + n

    val expected = Branch(Leaf(22), Branch(Leaf(42), Leaf(6)))

    val actual = Tree.fold[Int, Tree[Int]](t)(leaf => Leaf(double(leaf)), (l,r) => Branch(l,r))

    expected should equal(actual)
  }

}
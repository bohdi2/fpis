package chapter3

import org.scalatest.{FlatSpec, Matchers}
import scala.annotation.tailrec

class Exercises3Spec
  extends FlatSpec
  with Matchers {

  behavior of "Chapter3 List Basics"

  it should "sum must sum" in {
    val l = List(1,2,3,4,5)
    15 should equal(List.sum(l))
  }

  it should "product must multiply" in {
    val l = List(1,2,3,4,5)
    120 should equal(List.product(l))
  }

  it should "obey equals" in {
    List(1,2) should equal(List(1,2))
    List() should equal(Nil)
    List(1,2) should not equal List(1,2,3)
  }

  it should "tail should work" in {
    List(2,3) should equal(List.tail(List(1,2,3)))
  }

  it should "setHead should change the head" in {
    List(10,2) should equal(List.setHead(List(1,2), 10))
  }

  it should "drop the first n element" in {
    val l = List(1,2,3,4,5)
    List(3,4,5) should equal(List.drop(l, 2))
  }

  it should "dropWhile predicate is true" in {
    val l = List(1,2,3,4,5)
    val p = (n: Int) => n<4

    List(4,5) should equal(List.dropWhile(l, p))
  }

  "List.reverse" should "reverse" in {
    List(1,2,3,4) should equal(List.reverse(List(4,3,2,1)))
  }

  "List.init" should "return everything but the very end of the list" in {
    val l = List(1,2,3,4,5)

    List(1,2,3,4) should equal(List.init(l))
  }

  "List.sum2" should "add" in {
    val l = List(1,2,3,4,5)
    15 should equal(List.sum2(l))
  }

  "List.product2" should "multiply" in {
    val l: List[Double] = List(1.0,2,3,4,5)
    120 should equal(List.product2(l))
  }

  "List.length2" should "work with a non empty list" in {
    List.length2(List(1,2,3)) should equal(3)
  }


  "List.length2" should "work with an empty list" in {
    List.length2(List()) should equal(0)
  }

  @tailrec
  final def makeBigList(n: Long, l: List[Long]): List[Long] =
    if (n == 0) l
    else makeBigList(n-1, Cons(n-1, l))

  "foldRight" should "break the stack" in {
    //List.length(makeBigList(1000000, Nil:List[Long]))
  }

  "foldLeft" should "Sum a list" in {
    List.foldLeft(List(1,2,3), 10)(_+_) should equal(16)
  }

  "foldLeft" should "not break the stack" in {
    List.length3(makeBigList(1000000, Nil:List[Long]))
  }

  "List.sum3" should "sum" in {
    val l = List(1,2,3,4,5)
    15 should equal(List.sum3(l))
  }

  "List.product3" should "multiply" in {
    val l: List[Double] = List(1.0,2,3,4,5)
    120 should equal(List.product3(l))
  }

  "List.reverse3" should "reverse" in {
    List(1,2,3,4) should equal(List.reverse3(List(4,3,2,1)))
  }

  "List.bump" should "increment" in {
    List(2,3,4) should equal(List.bump(List(1,2,3)))
  }

  "List.cbump" should "append" in {
    List("ax", "bx") should equal(List.cbump(List("a", "b")))
  }

  "List.map" should "increment" in {
    List("1x","2x","3x") should equal(List.map[Int, String](List(1,2,3), _ + "x"))
  }


}

package chapter2

import org.scalatest.{FlatSpec, Matchers}

class Exercises2Spec
  extends FlatSpec
  with Matchers {

  behavior of "Chapter2 Exercise 2.2 isSorted"

  it should "return true with sorted arrays" in {
    val as = Array(1,2,3)
    true should equal(Exercies2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "return false with unsorted arrays" in {
    val as = Array(1,3,2)
    false should equal(Exercies2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "work with Arrays of size 1" in {
    val as = Array(1)
    true should equal(Exercies2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "work with Arrays of size 0" in {
    val as = Array[Int]()
    true should equal(Exercies2.isSorted[Int](as, (n, m) => n<m))
  }


  behavior of "Chapter2 Exercise 2.3 curry"

  it should "plus5" in {
    def add(n: Int, m: Int) = n + m

    val cAdd = Exercies2.curry(add)
    val plus5 = cAdd(5)
    15 should equal(plus5(10))
  }

  it should "prepend Hello" in {
    def add(n: String, m: String) = n + m

    val cAdd = Exercies2.curry(add)
    val plus5 = cAdd("Hello ")
    "Hello World" should equal(plus5("World"))
  }



  behavior of "Chapter2 Exercise 2.4 uncurry"

  it should "work" in {

    def makePlus(n: Int) = (m: Int) => n + m

    val plus = Exercies2.uncurry(makePlus)

    15 should equal(makePlus(5)(10))
    15 should equal(plus(5, 10))
  }

  it should "negate curry" in {
    def add(n: Int, m: Int) = n + m
    val add2 = Exercies2.uncurry(Exercies2.curry(add))

    add(7, -89) should equal(add2(7, -89))
  }

  behavior of "Chapter2 Exercise 2.5 compose"

  it should "work" in {

    def times3(n: Int) = 3*n
    def plus1(n: Int) = 1+n

    val f = Exercies2.compose(times3, plus1)

    18 should equal(f(5))  // 3*(5+1)
  }

  it should "handle multiple types" in {

    def concat3(n: Int) = s"$n$n$n"
    def length1(s: String) = s.length + 1

    val f = Exercies2.compose(length1, concat3)

     7 should equal(f(15))  // 3*(5+1)
  }



}

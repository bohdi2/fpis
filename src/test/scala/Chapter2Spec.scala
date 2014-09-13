
import org.scalatest.{FlatSpec, Matchers}

class Chapter2Spec
  extends FlatSpec
  with Matchers {

  behavior of "Chapter2 Exercise 2.2 isSorted"

  it should "return true with sorted arrays" in {
    val as = Array(1,2,3)
    true should equal(Chapter2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "return false with unsorted arrays" in {
    val as = Array(1,3,2)
    false should equal(Chapter2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "work with Arrays of size 1" in {
    val as = Array(1)
    true should equal(Chapter2.isSorted[Int](as, (n, m) => n<m))
  }

  it should "work with Arrays of size 0" in {
    val as = Array[Int]()
    true should equal(Chapter2.isSorted[Int](as, (n, m) => n<m))
  }


  behavior of "Chapter2 Exercise 2.3 curry"

  it should "plus5" in {
    def add(n: Int, m: Int) = n + m

    val cAdd = Chapter2.curry(add)
    val plus5 = cAdd(5)
    15 should equal(plus5(10))
  }

  it should "prepend Hello" in {
    def add(n: String, m: String) = n + m

    val cAdd = Chapter2.curry(add)
    val plus5 = cAdd("Hello ")
    "Hello World" should equal(plus5("World"))
  }




}

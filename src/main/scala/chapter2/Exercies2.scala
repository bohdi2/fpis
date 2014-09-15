package chapter2


object Exercies2 {

  // Exercise 2.2

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) = {

    def loop(n: Int): Boolean = {
      if (n >= (as.length - 1)) true
      else if (! ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }

  // Exercise 2.3

  def curry1[A,B,C](f: (A,B) => C): A => (B => C) = {
    def x(a: A) = {
      def y(b: B) = f(a,b)
      y _
    }
    x
  }

  def curry2[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => { (b: B) => f(a,b) }
  }

  def curry3[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  def curry4[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    x => y => f(x,y)
  }


  // Exercise 2.4

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}

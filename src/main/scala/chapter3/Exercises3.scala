package chapter3


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(as: List[Int]): Int = as match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(as: List[Int]): Int = as match {
    case Nil => 1
    case Cons(head, tail) => head * product(tail)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) => tail
  }

  def setHead[A](as: List[A], b: A) = Cons(b, tail(as))

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (f(head)) dropWhile(tail, f)
      else l
  }

  def reverse[A](l: List[A]): List[A] = {

    def r(a: List[A], b: List[A]): (List[A], List[A]) = b match {
      case Nil => (a, b)
      case Cons(head, tail) => r(Cons(head, a), tail)
    }

    r(Nil, l)._1
  }

  def init[A](l: List[A]): List[A] = {
    reverse(tail(reverse(l)))
  }

  def foldRight[A,B](as: List[A], accum: B)(f: (A,B) => B): B = as match {
    case Nil => accum
    case Cons(x, xs) => f(x, foldRight(xs, accum)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ds: List[Double]) = foldRight(ds, 1.0)(_ * _)

  def length2[A](l: List[A]) = foldRight(l, 0)((a,b) => b+1)

  def foldLeft[A,B](as: List[A], accum: B)(f: (B,A) => B): B = as match {
    case Nil => accum
    case Cons(x, xs) => foldLeft(xs, f(accum,x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ds: List[Double]) = foldLeft(ds, 1.0)(_ * _)

  def length3[A](l: List[A]) = foldLeft(l, 0)((b,a) => b+1)

  def reverse3[A](xs: List[A]): List[A] =
    foldLeft(xs, Nil:List[A])((accum,x) => Cons(x,accum))

  def bump(xs: List[Int]): List[Int] =
     foldRight(xs, Nil:List[Int])((x, z) => Cons(x + 1, z))

  def cbump(xs: List[String]): List[String] =
    foldRight(xs, Nil:List[String])((x, z) => Cons(x + "x", z))

  def map[A,B](as: List[A], f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a,z) => Cons(f(a), z))

}


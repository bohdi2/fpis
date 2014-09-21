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

  // append a1 to the end of a2
  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a2, a1)((x, accum) => Cons(x, accum))
    //a2

  def bump(xs: List[Int]): List[Int] =
     foldRight(xs, Nil:List[Int])((x, z) => Cons(x + 1, z))

  def dToS(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((d, z) => Cons(d.toString, z))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a,z) => Cons(f(a), z))

  def filter[A](as: List[A])(p: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((a,accum) => if (p(a)) Cons(a, accum) else accum)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val m:List[List[B]] = List.map(as)(f)
    foldRight(m, Nil:List[B])((accum, a) => append(a, accum))
  }

  def filter2[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)(a => if (p(a)) List(a) else Nil:List[A])

  def zipAdd(a1:List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
  }

  def zipWith[A,B](a1:List[A], a2: List[A])(f: (A,A) => B): List[B] = (a1, a2) match {
    case (Nil, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }


}


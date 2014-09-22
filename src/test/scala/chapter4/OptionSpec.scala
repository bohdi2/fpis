package chapter4

import org.scalatest.{FlatSpec, Matchers}
import scala.{Option => _, Some => _, Either => _, _}

class OptionSpec
  extends FlatSpec
  with Matchers {

  behavior of "Chapter4 Option"

  "Options Some.map" should "convert Some" in {
    Some(6) should equal(Some(3).map(_ * 2))
  }

  "Options None.map" should "do nothing" in {
    val out: Option[Int] = None
    None should equal(out.map(_ * 2))
  }

  def doubleIfEven(n: Int): Option[Int] = if (n%2 == 0) Some(2*n) else None

  "Options Some.flatMap" should "convert Some" in {
    Some(8) should equal(Some(4).flatMap(doubleIfEven))
    None should equal(Some(3).flatMap(doubleIfEven))
  }

  "Options None.flatMap" should "do nothing" in {
    val out: Option[Int] = None
    None should equal(out.flatMap(doubleIfEven))
  }


  "Options Some.getOrElse" should "Return Some's value" in {
    6 should equal(Some(6).getOrElse(55))
  }

  "Options None.getOrElse" should "Return default value" in {
    val out: Option[Int] = None
    55 should equal(out.getOrElse(55))
  }


  "Options Some.orElse" should "Return Some's value" in {
    Some(6) should equal(Some(6).orElse(Some(55)))
  }

  "Options None.orElse" should "Return default value" in {
    val out: Option[Int] = None
    Some(55) should equal(out.orElse(Some(55)))
  }


  "Options Some.filter" should "Return Some's value" in {
    Some(6) should equal(Some(6).filter(_ == 6))
    None should equal(Some(6).filter(_ != 6))

  }

  "Options None.filter" should "Return None" in {
    val out: Option[Int] = None
    None should equal(out.filter(n => true))
  }
}
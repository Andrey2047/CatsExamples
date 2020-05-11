package com.andrey2047.core

import cats.kernel.Semigroup
import cats.implicits._

import scala.util.Try

object MonoidExample {

  trait Monoid[A] {
    def mappend(a1: A, a2: A): A
    def mzero: A
  }

  object Monoid {
    implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }
    implicit val StringMonoid: Monoid[String] = new Monoid[String] {
      def mappend(a: String, b: String): String = a + b
      def mzero: String = ""
    }
  }

  def sum[A: Monoid](xs: List[A]): A = {
    val m = implicitly[Monoid[A]]
    xs.foldLeft(m.mzero)(m.mappend)
  }

  def main(args: Array[String]): Unit = {
    import Monoid._

    assert(sum(List(1,2,3,4)) == 10)

    assert(sum(List("ab", "bc")) == "abbc")

    println(Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)))

    println(Semigroup[Option[Int]].combine(Option(1), None))

    println(Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6))

    val map1 = Map("hello" -> 1, "world" -> 1)
    val map2 = Map("hello" -> 2, "cats"  -> 3)

    println(Semigroup[Map[String, Int]].combine(map1, map2))

    println(Semigroup[Try[Int]].combine(Try({throw new IndexOutOfBoundsException}),
      Try(throw new ArrayIndexOutOfBoundsException)))

    println(Semigroup[Option[Int]].combine(Some(1), Some(2)))

  }

}

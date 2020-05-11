package com.andrey2047.core

object WriterMonoid {

  type Writer[A] = (A, String)

  type Morphism[A, B] = A => Writer[B]

  def pure[A](x: A): Writer[A] = (x, "")

  implicit class KleisliOps[A, B](m1: A => Writer[B]) {
    def >=>[C](m2: B => Writer[C]): A => Writer[C] =
      x => {
        val (y, s1) = m1(x)
        val (z, s2) = m2(y)
        (z, s1 + s2)
      }
  }

  def main(args: Array[String]): Unit = {
    val upCase: String => Writer[String] =
      s => (s.toUpperCase, "upCase ")

    val toWords: String => Writer[List[String]] =
      s => (s.split(' ').toList, "toWords ")

    val process: String => Writer[List[String]] = {
      upCase >=> toWords
    }

    process.toString()

  }

}

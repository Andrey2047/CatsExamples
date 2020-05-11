package com.andrey2047.core

object Category {

  def identity[A](a: A): A = a

  def main(args: Array[String]): Unit = {
    identityFun()
    functionComposition()

    val k1: Nothing = k(2)

    println(k1)
  }

  def identityFun():Unit = {
    def identity[A](a: A): A = a
    val f: Int => Int = _ + 1
    assert(f.compose[Int](identity[Int])(2) == f(2))
  }

  def functionComposition(): Unit = {
    val f1: Int => Int = _ + 1
    val f2: Int => Int = _ + 2

    assert(f1.compose(f2)(3) == 6)

  }

  def k(p: Int): Nothing = {
    throw new Exception
  }

  def optToEither[A](a: Option[A]): Either[A, Unit] = {
    a match {
      case Some(k) => Left(k)
      case None => Right(())
    }
  }

  def eitherToOption[A](e: Either[A, Unit]) : Option[A] = {
    e match {
      case Left(k) => Some(k)
      case Right(_) => None
    }
  }

}

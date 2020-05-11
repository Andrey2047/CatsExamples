package com.andrey2047.core

import cats.Monad
import cats.instances.option._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.language.higherKinds

object MonadExample {

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x*x + y*y))

  def main(args: Array[String]): Unit = {

    val opt1 = Monad[Option].pure(3)
    // opt1: Option[Int] = Some(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

    // opt2: Option[Int] = Some(5)
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)

    println(sumSquare(Option(3), Option(4)))

  }

}

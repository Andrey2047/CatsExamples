package com.andrey2047.effects

import cats.data.{ Reader, Writer }
import cats.effect.IO
import cats.instances.try_._

import scala.util.Try

object MonadErrorExample {

  def main(args: Array[String]): Unit = {

    val exn: Throwable =
      new RuntimeException("It's all gone wrong")

    type Logged[A] = Writer[Vector[String], A]
    type ReaderM[A] = Reader[Int, Int]

    import cats.syntax.applicative._

    println(123.pure[Try].pure[Try])

    println(123.pure[IO])


  }

}

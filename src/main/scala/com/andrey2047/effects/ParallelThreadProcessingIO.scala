package com.andrey2047.effects

import cats.effect.{ ContextShift, IO }
import cats.implicits._

import scala.util.{ Failure, Success, Try }

object ParallelThreadProcessingIO {

  def main(args: Array[String]): Unit = {
    IO.pure(12).flatMap(x => IO{println(x)}).unsafeRunSync()

    val boom: IO[Unit] = IO.raiseError(new Exception("boom"))

    println(boom.attempt.unsafeRunSync())

    import scala.concurrent.ExecutionContext
    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    import cats.implicits._

    val program = (IO { calc(1, 1) }, IO { calc(2, 2) }, IO { calc(3, 0) }).parMapN {
      (r1, r2, r3) => {
        for {
          r_1 <- r1
          r_2 <- r2
          r_3 <- r3
        } yield {
          r_1 + r_2 + r_3
        }
      }
    }

    println(program.attempt.unsafeRunSync())

  }

  def calc(a: Int, b: Int): Either[Throwable, Int] = {
    println(Thread.currentThread().getName)

    Thread.sleep(10000)

    println(Thread.currentThread().getName + "waked up")
    Try {a/b} match {
      case Success(value) => value.asRight[Throwable]
      case Failure(exception) => exception.asLeft[Int]
    }
  }


}

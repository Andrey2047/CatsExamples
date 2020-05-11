package com.andrey2047.effects

import cats.effect.IO
import cats.effect.concurrent.Ref

object RefExample {

  def main(args: Array[String]): Unit = {
    val value = Ref[IO].of(1)
    val program = for {
      ref <- value
      printValue = ref.get.flatMap(i => IO(println(s"Current value is $i")))
      _ <- printValue
      _ <- ref.update(_ + 1)
      _ <- printValue
      _ <- ref.update(_ * 2)
      _ <- printValue
    } yield ()

    program.unsafeRunSync()

    println(value.unsafeRunSync().get.unsafeRunSync())

  }

}

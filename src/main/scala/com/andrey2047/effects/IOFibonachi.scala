package com.andrey2047.effects

import cats.effect.IO

object IOFibonachi {

  def fib(n: Int, a: Long, b: Long): IO[Long] =
    IO.suspend {
      if (n > 0)
        fib(n - 1, b, a + b)
      else
        IO.pure(a)
    }

  def main(args: Array[String]): Unit = {
    println(fib(5, 1, 1).unsafeRunSync())
  }

}

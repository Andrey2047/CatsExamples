package com.andrey2047.effects


object IOExample {

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def apply[A](a: => A): F[A] = unit(a)
  }

  sealed trait IO[A] {
    self =>
    def run(): A
    def ++(io: IO[A]): IO[A] = new IO[A] {
      def run(): A = { self.run(); io.run() }
    }

    def map[B](f: A => B): IO[B] = new IO[B] {def run(): B = f(self.run()) }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {def run(): B = f(self.run()).run() }

    def forever[A,B](a: IO[A]): IO[B] = {
      lazy val t: IO[B] = forever(a)
      a flatMap (_ => t)
    }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] {def run = a }
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    override def apply[A](a: => A): IO[A] = unit(a)
  }

  def ReadLine: IO[String] = IO { readLine }

  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  val prompt: IO[Unit] = PrintLine("Enter a temperature in degrees Fahrenheit: ")

  def main(args: Array[String]): Unit = {
    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()



    converter.forever(IO {println("d")}).run()
  }

}

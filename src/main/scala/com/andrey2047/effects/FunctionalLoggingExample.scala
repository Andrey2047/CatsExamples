package com.andrey2047.effects

import cats.Monad
import cats.data.Chain
import cats.effect.concurrent.Ref
import cats.effect.{ IO, Sync }
import cats.implicits._
import io.chrisdavenport.log4cats.MessageLogger
import io.chrisdavenport.log4cats.extras.{ LogLevel, LogMessage }
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

object FunctionalLoggingExample {

  object TestStateLogger {
    def apply(ref: Ref[IO, Chain[LogMessage]]): TestStateLogger = {
      new TestStateLogger(ref)
    }
  }

  class TestStateLogger(val ref: Ref[IO, Chain[LogMessage]]) extends MessageLogger[IO] {
    private def append(level: LogLevel, msg: String): IO[Unit] =
      ref.update(_ :+ LogMessage(level, None, msg))
    override def error(message: => String): IO[Unit] = append(LogLevel.Error, message)
    override def warn(message: => String): IO[Unit] = append(LogLevel.Warn, message)
    override def info(message: => String): IO[Unit] = append(LogLevel.Info, message)
    override def debug(message: => String): IO[Unit] = append(LogLevel.Debug, message)
    override def trace(message: => String): IO[Unit] = append(LogLevel.Trace, message)
  }

  class Calculator[F[_] : Monad : MessageLogger] {

    def doCalculation(a: Int, b: Int, multiplier: Int): F[Int] = {
      for {
        _ <- MessageLogger[F].info("Start processing")
        t1 <- add(a, b)
        result <- mul(t1, multiplier)
        _ <- MessageLogger[F].debug(s"Result of operation is $result")
        _ <- MessageLogger[F].info("Finished processing")
      } yield {
        result
      }
    }

    def add(a: Int, b: Int): F[Int] = {
      for {
        result <- (a + b).pure[F]
        _ <- MessageLogger[F].debug(s"Added two integers: $result")
      } yield result
    }

    def mul(a: Int, b: Int): F[Int] = {
      if (a == 0 || b == 0) {
        MessageLogger[F].warn("One of arguments is zero. Result will be 0") *> 0.pure[F]
      } else {
        for {
          result <- (a * b).pure[F]
          _ <- MessageLogger[F].debug(s"Multiplied two integers: $result")
        } yield result
      }
    }
  }

  object Program {

    def process[F[_] : Sync]: F[Unit] = {
      implicit val refLogger: MessageLogger[F] = Slf4jLogger.getLoggerFromClass(Program.getClass)
      val someService = new Calculator[F]
      someService.doCalculation(1, 2, 3).flatMap(println(_).pure[F])
    }

  }

  object CalculatorTest {

    final val ExpectedLogMessages: List[LogMessage] = List(
      LogMessage(LogLevel.Info, None, "Start processing"),
      LogMessage(LogLevel.Debug, None, "Added two integers: 3"),
      LogMessage(LogLevel.Debug, None, "Multiplied two integers: 9"),
      LogMessage(LogLevel.Debug, None, "Result of operation is 9"),
      LogMessage(LogLevel.Info, None, "Finished processing"),
    )


    def main(args: Array[String]): Unit = {
      val testDoCalculation = Ref.of[IO, Chain[LogMessage]](Chain.empty).flatMap(ref => {
        implicit val refLogger: MessageLogger[IO] = TestStateLogger(ref)
        val service = new Calculator[IO]
        for {
          methodResult <- service.doCalculation(1, 2, 3)
          stateRes <- ref.get
          _ <- IO { assert(methodResult == 9) }
          _ <- IO {
            println(stateRes.toList)
            println(ExpectedLogMessages)
            assert(stateRes.toList.equals(ExpectedLogMessages))
          }
        } yield ()
      })

      testDoCalculation.unsafeRunSync()
    }
  }

  def main(args: Array[String]): Unit = {
    Program.process[IO].unsafeRunSync()
  }
}
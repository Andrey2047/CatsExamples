package com.andrey2047.core

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{ Await, CanAwait, Future }
import scala.language.postfixOps
import scala.util.{ Failure, Success, Try }
import scala.concurrent.duration._


object EitherTExample {

  def main(args: Array[String]): Unit = {

    def parseDouble(s: String): Either[String, Double] =
      Try(s.toDouble).map(Right(_)).getOrElse(Left(s"$s is not a number"))

    def divide(a: Double, b: Double): Either[String, Double] =
      Either.cond(b != 0, a / b, "Cannot divide by zero")

    def divisionProgram(inputA: String, inputB: String): Either[String, Double] =
      for {
        a <- parseDouble(inputA)
        b <- parseDouble(inputB)
        result <- divide(a, b)
      } yield result

    divisionProgram("4", "2") // Right(2.0)
    divisionProgram("a", "b") //

    def parseDoubleAsync(s: String): Future[Either[String, Double]] =
      Future.successful(parseDouble(s))

    def divideAsync(a: Double, b: Double): Future[Either[String, Double]] =
      Future.successful(divide(a, b))

    def divisionProgramAsync(inputA: String, inputB: String): Future[Either[String, Double]] =
      parseDoubleAsync(inputA) flatMap { eitherA =>
        parseDoubleAsync(inputB) flatMap { eitherB =>
          (eitherA, eitherB) match {
            case (Right(a), Right(b)) => divideAsync(a, b)
            case (Left(err), _) => Future.successful(Left(err))
            case (_, Left(err)) => Future.successful(Left(err))
          }
        }
      }

    for {
      a1 <- parseDoubleAsync("2")
      b1 <- parseDoubleAsync("3")
    } yield {
      (a1, b1) match {
        case (Right(a), Right(b)) => divideAsync(a, b)
        case (Left(err), _) => Future.successful(Left(err))
        case (_, Left(err)) => Future.successful(Left(err))
      }
    }

    divisionProgramAsync("4", "2")
  }

  def main2(args: Array[String]): Unit = {

    val f1: Future[Either[Throwable, Int]] = Future {
      Try(2) match {
        case Success(something) => Right(something)
        case Failure(err) => Left(err)
      }
    }

    val f2: Future[Either[Throwable, Int]] = Future {
      Try {
        throw new IllegalArgumentException
      } match {
        case Success(something) => Right(something)
        case Failure(err) => Left(err)
      }
    }

    val f = (for {
      f1v <- EitherT[Future, Throwable, Int](f1)
      f2v <- EitherT[Future, Throwable, Int](f2)
    } yield {
      f1v + f2v
    }).value

    Await.result(f, 2 seconds) match {
      case Right(b) => b
      case Left(a) => throw a
    }
  }


}

package com.andrey2047.effects

import cats.effect.concurrent.Ref
import cats.{ Applicative, Monad }
import cats.effect.{ Async, IO, Sync }
import cats.implicits._
import cats.syntax._
import sun.net.www.http.HttpClient

import scala.concurrent.Future
import scala.util.{ Failure, Success }

object ConsoleWithIOEffects {

  trait Console[F[_]] {
    def putStrLn(str: String) : F[Unit]
    def readLn: F[String]
  }

  class StdConsole[F[_] : Sync] extends Console[F] {
    override def putStrLn(str: String): F[Unit] = Sync[F].delay(println(str))
    override def readLn: F[String] = Sync[F].delay(scala.io.StdIn.readLine)
  }

//  class RemoteConsole[F[_] : Async] extends Console[F] {
//
//    private def fromFuture[A](fa: F[Future[A]]): F[A] = {
//      fa.flatMap {
//        future => {
//          Async[F].async { cb =>
//            future.onComplete{
//              case Success(value) => cb(Right(value))
//              case Failure(e) => cb(Left(e))
//            }
//          }
//        }
//      }
//    }
//
//    override def putStrLn(str: String): F[Unit] = fromFuture(Sync[F].delay(HttpClient)
//    override def readLn: F[String] = fromFuture(Sync[F].deplay(HttpClient.get)))
//  }

  class TestConsole[F[_]: Applicative](state: Ref[F, List[String]]) extends Console[F] {
    override def putStrLn(str: String): F[Unit] = state.update(_ :+ str)
    override def readLn: F[String] = "test".pure[F]
  }

  def program[F[_]: Monad](implicit C: Console[F]) : F[Unit] = {
    for {
      _ <- C.putStrLn("Enter your name")
      n <- C.readLn
      _ <- C.putStrLn(s"Hello $n!")
    } yield ()
  }


  def main(args: Array[String]): Unit = {

  }

}

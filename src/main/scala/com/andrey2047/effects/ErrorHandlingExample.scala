package com.andrey2047.effects

import cats.MonadError
import cats.implicits._


object ErrorHandlingExample {

  case class User(id: String)
  case class Project(id: String)

  def parseUser[F[_]](s: String)(implicit F: MonadError[F, Error]) : F[User] = {
    if(s.startsWith("1")){
      F.pure(User("1"))
    } else {
      F.raiseError(new Error("some error"))
    }
  }

  def findUser[F[_]](id: String)(implicit F: MonadError[F, Error]): F[List[Project]] = ???

  def main(args: Array[String]): Unit = {
//    val u: Either[Error, User] = for{
//      user <- parseUser("")
//    } yield user
//
//    println(u)


  }

}

package com.andrey2047.core

import cats.data.{ EitherT, OptionT }
import cats.implicits._

object MonadTransformer {

  case class User(userName: String, age : Int)

  type ErrorOr[A] = Either[String, A]

  case class Item(id: Int, name: String)

  case class ProcessedItem(id: Int, name: String)

  case class WithInnerItems(items: Seq[Item])

  def extractItems(json: String): Either[Throwable, WithInnerItems] = ???

  def processItems(json: String): Either[Throwable, Seq[ProcessedItem]] = {
    Right(Seq())
  }

  def main(args: Array[String]): Unit = {

      val k: Either[Throwable, Item] = Item(1, "").asRight[Throwable]
      val s1: List[Either[Throwable, Item]] = List(Right(Item(1, "sss")), Right(Item(2, "sssd")))

      val result = for {
        l <- EitherT.fromEither[List](k)
        s <- EitherT[List, Throwable, Item](s1)
      } yield s.id



      val eiUser: ErrorOr[Some[User]] = Right(Some(User("uuu", 27)))
      val eiUser2: ErrorOr[Some[User]] = Right(Some(User("uuu", 22)))

      val f = for {
        e1 <- OptionT[ErrorOr, User](eiUser)
        e2 <- OptionT[ErrorOr, User](eiUser2)
      } yield e1.age + e2.age

      println(f.value)





  }

}

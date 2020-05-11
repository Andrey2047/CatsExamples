package com.andrey2047.core

import java.time.ZonedDateTime
import cats.implicits._

object BiFunctor {

  case class DomainError(message: String)

  def dateTimeFromUser: Either[Throwable, ZonedDateTime] =
    Right(ZonedDateTime.now())


  def main(args: Array[String]): Unit = {
    dateTimeFromUser.bimap(
      error => DomainError(error.getMessage),
      dateTime => dateTime.toEpochSecond
    )
  }

}

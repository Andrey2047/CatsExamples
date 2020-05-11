package com.andrey2047.core

import cats.data.{ Validated, ValidatedNel }
import cats.implicits._

object TraverseExample {

  type XorStr[A] = Either[NumberFormatException, A]

  def parseIntEither(s: String): XorStr[Int] =
    Either.catchOnly[NumberFormatException](s.toInt)

  def parseIntValidated(s: String): ValidatedNel[NumberFormatException, Int] =
    Validated.catchOnly[NumberFormatException](s.toInt).toValidatedNel

  def main(args: Array[String]): Unit = {

    println(List("1", "1", "3").traverse(parseIntEither))

  }

}

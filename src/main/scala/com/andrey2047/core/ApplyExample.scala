package com.andrey2047.core

import cats.data.Kleisli
import cats.{ Apply, Functor, Id }
import cats.implicits._
import com.andrey2047.core.ApplyExample.C

object ApplyExample {

  type T[A] = Kleisli[Id, C, A]

  case class C(s1: String, s2: String, s3: String)

  def f1: Kleisli[Id, C, String] = Kleisli[Id, C, String]{ _.s1 }
  def f2: Kleisli[Id, C, String] = Kleisli[Id, C, String]{ _.s1 }
  def f3: Kleisli[Id, C, String] = Kleisli[Id, C, String]{ _.s1 }


  def main(args: Array[String]): Unit = {

    Apply[T].map3(f1, f2, f3)((x1,x2,x3) => C(x1,x2,x3))

    println(Functor[Option].map(Option("Hello"))(_.length))
    println(Apply[Option].map(Option("Hello"))(_.length))
    println(Apply[Option].ap(None)(Option("Hello")))

    val f = (x: Int) => (y: Int) => x + y

    val result = Some(42).map(f)

    println(Apply[Option].ap(result)(Some(33)))


  }


}

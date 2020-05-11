package com.andrey2047.core

import cats.Functor
import cats.implicits._


object FunctorExample {

  def main(args: Array[String]): Unit = {

    println(Functor[Option].map(Option("Hello"))(_.length))
    println(Functor[Option].map(None: Option[String])(_.length))

    val lenOption: Option[String] => Option[Int] =
      Functor[Option].lift(_.indexOf("b"))

    println(lenOption(Option("abc")))

    val source = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    println(product)

    val listOpt = Functor[List] compose Functor[Option]

    val exampleList = List(Some(1), None, Some(3))

    println(listOpt.map(exampleList)(_ + 1)) // with cats

    val f: Int => Int = _ + 1

    val resultList = exampleList.map(x => {
        for {x1 <- x} yield x1 + 1
      }
    )

    println(resultList)
    println(Functor[List].lift[String, Int](_.length)(List("a", "bb", "ccc")))
    println(Functor[Option].map(Some(2))(_ * 2))

    println(Option(1) |+| None)

  }

}

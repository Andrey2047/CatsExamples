package com.andrey2047.core

import cats.Show
import cats.implicits._

object ConCovariantFunctorsExample {

  case class Money(amount: Int)

  case class Salary(size: Money)

  implicit val showMoney: Show[Money] = Show.show(m => s"$$${m.amount}")

  implicit val showSalary: Show[Salary] = showMoney.contramap(_.size)

  def main(args: Array[String]): Unit = {
    println(Salary(Money(1000)).show)
  }

}

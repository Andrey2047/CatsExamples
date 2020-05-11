package com.andrey2047.core

import cats.data.Writer
import cats.implicits._

object WriterMonadExample {

  class Calculator {
    def add(a: Int, b: Int): Writer[Vector[String], Int] = {
      val sum = a+b
      Writer(Vector(s"Added two integers: $sum"), sum)
    }

    def mul(a: Int, b: Int): Writer[Vector[String], Int] = {
      val multiplication = a * b
      Writer(Vector(s"Multiplied two integers: $multiplication"), multiplication)
    }

    def doCalculation(a: Int, b:Int, multiplier: Int): Writer[Vector[String], Int] = {
      for {
        t1 <- add(a,b)
        result <- mul(t1, multiplier)
        _ <- Writer.tell(Vector(s"Result of operation is ${result}"))
      } yield {
        result
      }
    }
  }


  object TestCalculator {

    def testDoCalculation(): Unit = {

      val (logs, result) = new Calculator().doCalculation(2, 3, 2).run

      assert(result == 10)
      assert(logs.size == 3)
      assert(logs(0) == "Added two integers: 5")
      assert(logs(1) == "Multiplied two integers: 10")
      assert(logs(2) == "Result of operation is 10")
    }
  }

  def main(args: Array[String]): Unit = {
    TestCalculator.testDoCalculation()


  }

}

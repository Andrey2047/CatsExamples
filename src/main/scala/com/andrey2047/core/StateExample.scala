package com.andrey2047.core

import cats.data.State

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.Future

object StateExample {

  final case class Robot(
      id: Long,
      sentient: Boolean,
      name: String,
      model: String)

  val rng = new scala.util.Random(0L)

  def createRobot: State[Seed, Robot] = {
    for {
      id <- nextLong
      sentient <- nextBoolean
      isCatherine <- nextBoolean
      name = if (isCatherine) "Catherine" else "Carlos"
      isReplicant <- nextBoolean
      model = if (isReplicant) "replicant" else "borg"
    } yield Robot(id, sentient, name, model)
  }

  final case class Seed(long: Long) {
    def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
  }

  final case class AsyncSeed(long: Long) {
    def next = Future(AsyncSeed(long * 6364136223846793005L + 1442695040888963407L))
  }

  val nextLong: State[Seed, Long] = State(seed =>
    (seed.next, seed.long))

  val nextBoolean: State[Seed, Boolean] = nextLong.map(long =>
    long > 0)

  def main(args: Array[String]): Unit = {

    val initialSeed = Seed(13L)

    println(createRobot.run(initialSeed).value)


  }

}

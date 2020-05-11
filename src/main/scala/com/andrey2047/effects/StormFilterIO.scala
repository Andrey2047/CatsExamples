package com.andrey2047.effects

import cats.effect.{ ContextShift, IO, Sync }
import cats.implicits._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

import scala.concurrent.ExecutionContext
import scala.util.{ Failure, Success, Try }


object StormFilterIO {

  sealed trait Tile

  implicit def unsafeLogger[F[_]: Sync] = Slf4jLogger.getLogger[F]

  case class Tile1() extends Tile
  case class Tile2() extends Tile
  case class Tile3() extends Tile

  case class RoadObject()

  case class TileContext(tileId: String, tile1: Seq[Tile1], tile2: Seq[Tile2], tile3: Seq[Tile3])

  def buildRoadObjects[F[_]: Sync](tileContext: TileContext): F[Either[Throwable, Seq[RoadObject]]] = {
    Logger[F].info("read road objects") *> Seq[RoadObject]().asRight[Throwable].pure[F]
  }

  trait PartitionReader {
    def parse[T](layer: String, tile: Seq[String]): Seq[T]
  }

  def readTile[T](layer: String, tiles: Seq[String])(implicit partitionReader: PartitionReader): Either[Throwable, Seq[T]] =
    Try { partitionReader.parse[T](layer, tiles) } match {
      case Success(value) => value.asRight[Throwable]
      case Failure(exception) => exception.asLeft[Seq[T]]
    }

  def main(args: Array[String]): Unit = {

    implicit val partitionReader = new PartitionReader {
      override def parse[T](layer: String, tile: Seq[String]): Seq[T] = {
        layer match {
          case "layer1" => Seq()
          case "layer2" => Seq()
          case _ => throw new IllegalArgumentException("illegal argument")
        }
      }
    }

    implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

    val tile1Retrieve1 = IO { readTile[Tile1]("layer1", Seq("1", "2")) }
    val tile1Retrieve2 = IO { readTile[Tile2]("layer2", Seq("1", "2")) }
    val tile1Retrieve3 = IO { readTile[Tile3]("layer3", Seq("1", "2")) }

    val program = (tile1Retrieve1, tile1Retrieve2, tile1Retrieve3).parMapN((r1, r2, r3) => {
      for {
        r_1 <- r1
        r_2 <- r2
        r_3 <- r3
      } yield {
        buildRoadObjects[IO](TileContext("", r_1, r_2, r_3))
      }
    })

    println(program.unsafeRunSync())
  }


}

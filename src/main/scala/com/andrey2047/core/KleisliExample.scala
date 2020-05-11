package com.andrey2047.core

import cats.Apply
import cats.data.Kleisli
import cats.implicits._
import cats.syntax._
import com.andrey2047.core.KleisliExample.MyEither

object KleisliExample {

  type MyEither[A] = Either[Throwable, A]

  val parse: String => MyEither[Int] =
    s => Either.catchNonFatal(s.toInt)

  val parseK: Kleisli[MyEither, String, Int] =
    Kleisli(parse)

  val reciprocal: Int => MyEither[Double] =
    i => Either.catchNonFatal(1.0/i)

  val reciprocalK: Kleisli[MyEither, Int, Double] =
    Kleisli(reciprocal)

  val parseAndReciprocal: Kleisli[MyEither,String,Double] =
    reciprocalK.compose(parseK)

  object ConfigurationExample {

    trait Config {
      def getCatalog1Name: MyEither[String]
      def getCatalog1Version: MyEither[Long]
    }

    def liftToK[T](f: Config => MyEither[T]): Kleisli[MyEither, Config, T] = Kleisli(f)

    case class Catalog(name: String, version: Long)
    case class CatalogService(cat1: Catalog, cat2: Catalog)

    type KOptionConfig[A] = Kleisli[MyEither, Config, A]

    type Ttt = (String, Long)

    def serviceWithApplicative: KOptionConfig[Catalog] = {

      type createCatalogT = (String, Long) => Catalog

      val createCatalog: Config => MyEither[(String, Long) => Catalog] = {
        _:Config => (Catalog(_, _)).asRight
      }

      val createCatalogK: KOptionConfig[createCatalogT] =
        Kleisli(createCatalog)

      val getName: Config => MyEither[String] = c => c.getCatalog1Name
      val getVersion: Config => MyEither[Long] = c => c.getCatalog1Version

      val getNameK: KOptionConfig[String] = Kleisli(getName)
      val getVersionK: KOptionConfig[Long] = Kleisli(getVersion)

      Apply[KOptionConfig].ap2(createCatalogK)(getNameK, getVersionK)

    }

    def serviceWithMap2: KOptionConfig[Catalog] = {

      val getName: Config => MyEither[String] = c => c.getCatalog1Name
      val getVersion: Config => MyEither[Long] = c => c.getCatalog1Version

      val getNameK: KOptionConfig[String] = Kleisli(getName)
      val getVersionK: KOptionConfig[Long] = Kleisli(getVersion)

      Apply[KOptionConfig].map2(getNameK, getVersionK)(Catalog)
    }

    def serviceWithLocal: KOptionConfig[Catalog] = {
      type Co[T] = Config => MyEither[T]
      type KleisliType[A] = Kleisli[MyEither, Config, A]

      type K1[A] = Config => MyEither[A]

      Apply[KleisliType].map2(Kleisli(_.getCatalog1Name), Kleisli(_.getCatalog1Version))(Catalog)
    }


  }

  def main(args: Array[String]): Unit = {

    val sampleConfig = new ConfigurationExample.Config {
      override def getCatalog1Name: MyEither[String] = "21221".asRight[Throwable]
      override def getCatalog1Version: MyEither[Long] = 12L.asRight[Throwable]
    }

    val sampleWitheErrorConfig = new ConfigurationExample.Config {
      override def getCatalog1Name: MyEither[String] = Either.left(new Throwable("error"))
      override def getCatalog1Version: MyEither[Long] = 12L.asRight[Throwable]
    }


    println(ConfigurationExample.serviceWithApplicative(sampleConfig))
    println(ConfigurationExample.serviceWithMap2(sampleWitheErrorConfig))


  }

}

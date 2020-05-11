package com.andrey2047.core

import cats.kernel.Semigroup
import cats.implicits._


object SemigroupExample {

  trait AnimalShelter {
    def mainAnimals: Map[String, Long]
    def additionalAnimals: Map[String, Long]
  }

  case class ArizonaAnimalShelter(mainAnimals: Map[String, Long], additionalAnimals: Map[String, Long])
    extends AnimalShelter

  def main(args: Array[String]): Unit = {

    println(Semigroup[AnimalShelter => Map[String, Long]]
      .combine(_.mainAnimals, _.additionalAnimals)
      (ArizonaAnimalShelter(Map("cat" -> 3, "dog" -> 4), Map("cat" -> 7, "dog" -> 12, "fish" -> 4))))
  }


}

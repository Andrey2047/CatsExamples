package com.andrey2047.other

//Type class consist of 3 components: type class, type class instances, type class interface

//type class
trait ShowAsLink[T] {
  def toLink(s: T): String
}

//type class instances
object ShowAsLinkInstances {

  implicit val StringShowAsLink:ShowAsLink[String] =
    (s: String) => s"<a href=''>String is ${ s.toString }</>"

  implicit val IntShowAsLink:ShowAsLink[Int] =
    (s: Int) => s"<a href=''>Int is ${ s.toString }</>"

  implicit def optionShowAsLink[A: ShowAsLink]: ShowAsLink[Option[A]] = {
    case Some(v:A) => implicitly[ShowAsLink[A]].toLink(v)
    case None => "None"
  }

//  implicit def listShowAsLink[A: ShowAsLink]: ShowAsLink[List[A]] =
//    (a: List[A]) => a.map(implicitly[ShowAsLink[A]].toLink(_)).mkString(",")

}

//type class interface
object ShowAsLinkObj {

  def showAsLink[A](value: A)(implicit w: ShowAsLink[A]): String =
    w.toLink(value)

  def showAsLink2[A : ShowAsLink](value: A): String = {
    implicitly[ShowAsLink[A]].toLink(value)
  }
}

//Also can be used syntax which extends method of base class
object ShowAsLinkSyntax {

  implicit class ExtendedWithShowAsLink[A](value: A) {
    def showAsL(implicit a: ShowAsLink[A]): String = {
      a.toLink(value)
    }
  }

}

object TypeClassExample {

  def main(args: Array[String]): Unit = {

    import ShowAsLinkInstances._

    println(ShowAsLinkObj.showAsLink("hello"))
    println(ShowAsLinkObj.showAsLink(1))

    import ShowAsLinkSyntax._

    println("hello".showAsL)
    println(1.showAsL)

    println(Option(12).showAsL)

  }

}
package io.hacksoft.xml

import scala.collection.generic.CanBuildFrom

trait XmlReads[T] {
  def reads(xml: XmlValue): XmlResult[T]
}

object XmlReads {
  def apply[T](f: XmlValue => XmlResult[T]): XmlReads[T] = (o: XmlValue) => f(o)

  def toObject[T](xml: XmlValue)(implicit r: XmlReads[T]): XmlResult[T] = r.reads(xml)

  private def simpleReads[T](message: String)(f: PartialFunction[XmlValue, T]): XmlReads[T] =
    XmlReads[T] {
      f.lift.andThen {
        case Some (x) => XmlSuccess(x)
        case None => XmlError(s"XmlReads error $message")
      }
    }

  implicit val booleanReads: XmlReads[Boolean] = simpleReads("boolean"){ case XmlLiteral(x) => x.toBoolean }
  implicit val intReads: XmlReads[Int] = simpleReads("int"){ case XmlLiteral(x) => x.toInt }
  implicit val stringReads: XmlReads[String] = simpleReads("string"){ case XmlLiteral(x) => x }
  implicit val charReads: XmlReads[Char] = simpleReads("char"){ case XmlLiteral(x) => x.charAt(0) }
  implicit val doubleReads: XmlReads[Double] = simpleReads("double"){ case XmlLiteral(x) => x.toDouble }
  implicit val floatReads: XmlReads[Float] = simpleReads("float"){ case XmlLiteral(x) => x.toFloat }
  implicit def traversableReads[F[_], A](implicit cbf: CanBuildFrom[F[_], A, F[A]], r: XmlReads[A]): XmlReads[F[A]] = XmlReads[F[A]]{
    case x: XmlObject =>
      val builder = cbf()
      builder.sizeHint(x.children.length)

      // TODO: fix this to return proper errors
      x.children.map {
        c => r.reads(c) match {
          case XmlSuccess(v) => builder += v
          case e: XmlError => throw new RuntimeException("Error while parsing xml to object " + e)
        }
      }
      XmlSuccess(builder.result())
    case _ => XmlError("Cannot read element into a list")
  }
}

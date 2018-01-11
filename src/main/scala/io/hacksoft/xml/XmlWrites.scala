package io.hacksoft.xml

import reflect.runtime.universe._

case class listElementName(name: String) extends scala.annotation.StaticAnnotation

// Contravariant because of `traversableReads`
trait XmlWrites[-A] {
  def writes(o: A): XmlValue
}

object XmlWrites {
  def apply[A](f: A => XmlValue): XmlWrites[A] = (o: A) => f(o)

  implicit val booleanWrites: XmlWrites[Boolean] = XmlLiteral(_)
  implicit val intWrites: XmlWrites[Int] = XmlLiteral(_)
  implicit val stringWrites: XmlWrites[String] = XmlLiteral(_)
  implicit val charWrites: XmlWrites[Char] = XmlLiteral(_)
  implicit val doubleWrites: XmlWrites[Double] = XmlLiteral(_)
  implicit val floatWrites: XmlWrites[Float] = XmlLiteral(_)
  implicit def traversableWrites[T](implicit w: XmlWrites[T], s: XmlSettings[T] = XmlSettings.default): XmlWrites[Traversable[T]] = {x =>
    XmlObject(s.listName, x.map(w.writes).toSeq)
  }

  implicit def optionWrites[T](implicit w: XmlWrites[T]): XmlWrites[Option[T]] = {
    case Some(y: T) => w.writes(y)
    case _ => XmlNull
  }
}


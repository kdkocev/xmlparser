package io.hacksoft.xml

trait XmlResult[+A] {
  def get: A

  def map[E](f: A => E): XmlResult[E] = this match {
    case XmlSuccess(v) => XmlSuccess(f(v))
    case e: XmlError => e
  }
}

case class XmlSuccess[A](get: A) extends XmlResult[A]
case class XmlError(error: String) extends XmlResult[Nothing] {
  def get: Nothing = throw new NoSuchElementException
}

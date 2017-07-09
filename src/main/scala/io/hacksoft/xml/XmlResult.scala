package io.hacksoft.xml

trait XmlResult[+A] {
  def get: A
}

case class XmlSuccess[A](get: A) extends XmlResult[A]
case class XmlError(error: String) extends XmlResult[Nothing] {
  def get: Nothing = throw new NoSuchElementException
}

package io.hacksoft.xml

trait XmlFormat[A] extends XmlReads[A] with XmlWrites[A]

object XmlFormat {
  def apply[A](r: XmlValue => XmlResult[A], w: A => XmlValue): XmlFormat[A] = new XmlFormat[A] {
    def reads(xml: XmlValue): XmlResult[A] = r(xml)

    def writes(o: A): XmlValue = w(o)
  }

  implicit def generalFormat[A](implicit r: XmlReads[A], w: XmlWrites[A]): XmlFormat[A] = XmlFormat(r.reads, w.writes)
}

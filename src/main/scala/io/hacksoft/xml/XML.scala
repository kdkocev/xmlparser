package io.hacksoft.xml

object XML {
  def parse[T](xml: String)(implicit r: XmlReads[T]): XmlResult[T] = XmlParser.parse(xml).as[T]
  def render[T](o: T)(implicit w: XmlWrites[T]): String = w.writes(o).toString
  def write[T](o: T)(implicit w: XmlWrites[T]): XmlValue = w.writes(o)
  def read[T](xml: XmlValue)(implicit r: XmlReads[T]): XmlResult[T] = r.reads(xml)
}

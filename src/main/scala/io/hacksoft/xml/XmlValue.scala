package io.hacksoft.xml

trait XmlValue

case class XmlObject(
  label: String,
  children: Seq[XmlValue] = Seq.empty,
  attributes: Seq[XmlAttribute] = Seq.empty,
  namespace: Option[Namespace] = None,
  minimiseEmpty: Boolean = false
) extends XmlValue {

  override def toString: String = render

  private def render: String = {
    val attributesEmptySpace = if(attributes.isEmpty) "" else " "
    val attributesString = attributes.mkString(" ")
    // If there is a literal with a literal after it - there should be an empty space between them
    val childrenString = children.foldLeft("") {
      case (res, x) if res.endsWith(">") || res == "" => res + x.toString
      case (res, x: XmlLiteral) => res + " " + x.toString
      case (res, x) => res + x.toString
    }

    if (minimiseEmpty && children.isEmpty) s"<$label$attributesEmptySpace$attributesString/>"
    else s"<$label$attributesEmptySpace$attributesString>$childrenString</$label>"
  }
}

case class XmlLiteral(value: String) extends XmlValue {
  override def toString: String = value
}

case class XmlAttribute(
  label: String,
  value: String,
  namespace: Option[Namespace] = None
) {
  override def toString: String = s"""$label="$value""""
}
object XmlAttribute {
  def fromMap(m: Map[String, String]): Seq[XmlAttribute] =
    m.map{case (x,y) => XmlAttribute(x,y)}.toSeq
}

// TODO: finish
case class Namespace(url: java.net.URL)

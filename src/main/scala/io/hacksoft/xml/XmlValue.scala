package io.hacksoft.xml

import scala.annotation.{implicitNotFound, tailrec}

@implicitNotFound("Cannot find implicit for {T} try defining XmlFormat")
trait XmlValue {
  def as[T](implicit r: XmlReads[T]): XmlResult[T] = r.reads(this)
  def \(name: String): Seq[XmlValue]
  def \! (name: String): XmlValue
  def \!? (name: String): Option[XmlValue]
  def \@ (name: String): XmlAttribute
  def \@? (name: String): Option[XmlAttribute]
}

case class XmlObject(
  label: String,
  children: Seq[XmlValue] = Seq.empty,
  attributes: Seq[XmlAttribute] = Seq.empty,
  namespace: Option[Namespace] = None,
  minimiseEmpty: Boolean = false
) extends XmlValue {

  override def \ (name: String): Seq[XmlValue] = children.filter{
    case x: XmlObject if x.label == name => true
    case _ => false
  }

  override def \! (name: String): XmlValue = (this \ name) match {
    case List(el) => el
    case Nil => throw new NoSuchElementException
    case _ => throw new RuntimeException(s"More than one element with name $name found")
  }

  override def \!? (name: String): Option[XmlValue] = (this \ name) match {
    case List(el) => Some(el)
    case Nil => None
    case _ => throw new RuntimeException(s"More than one element with name $name found")
  }

  override def \@ (name: String): XmlAttribute = attributes.filter(x => x.label == name) match {
    case List(x) => x
    case Nil => throw new NoSuchElementException
    case _ => throw new RuntimeException("Attributes should have different names")
  }

  override def \@? (name: String): Option[XmlAttribute] = attributes.filter(x => x.label == name) match {
    case List(x) => Some(x)
    case Nil => None
    case _ => throw new RuntimeException("Attributes should have different names")
  }

  private def constructAttributesString: String = {
    val attributesEmptySpace = if(attributes.isEmpty) "" else " "
    val attributesString = attributes.mkString(" ")
    attributesEmptySpace + attributesString
  }

  /**
    * Recursive calling XmlValue.toString to render complex children
    * If there is a literal with a literal after it - there should be
    * an empty space between them
    */
  private def constructChildrenString: String =
    children.foldLeft("") {
      case (res, x) if res.endsWith(">") || res == "" => res + x.toString
      case (res, x: XmlLiteral) => res + " " + x.toString
      case (res, x) => res + x.toString
    }

  /**
    * TODO: Recursively find all the namespaces and decide which ones should be in the current layer
    */
  private def constructNamespacesString: String = {
    def allNamespaces(ns: Option[Namespace]): List[Namespace] = {
      // Get recursive parent namespaces
      @tailrec
      def iter(ns: Option[Namespace], res: List[Namespace]): List[Namespace] = ns match {
        case None => res
        case Some(namespace) => iter(namespace.parent, namespace :: res)
      }
      // Add all namespaces from attributes
      val attributesNamespaces = attributes.collect{case XmlAttribute(_, _, Some(ns)) => ns}.toList

      def distinctNamespaces(l: List[Namespace]): List[Namespace] =
        l.groupBy(x => (x.uri, x.prefix)).map(_._2.head).toList

      distinctNamespaces((iter(ns, Nil) ++ attributesNamespaces))
    }

    val allNs = allNamespaces(namespace).filter(_.visibleInNode)
    val leadingWhitespace = if(allNs.isEmpty) "" else " "

    val namespacesString = allNs.map{
      case Namespace(uri, Some(prefix), _, _) => s"""xmlns:$prefix="$uri""""
      case Namespace(uri, None, _, _) => s"""xmlns="$uri""""
    }.mkString(" ")

    leadingWhitespace + namespacesString
  }

  private def render: String = {
    val attributesString = constructAttributesString
    val childrenString = constructChildrenString
    val namespacesString = constructNamespacesString
    val prefix = namespace.flatMap(x => x.prefix.map(y => y + ":")).getOrElse("")

    if (minimiseEmpty && children.isEmpty) s"<$prefix$label$attributesString$namespacesString/>"
    else s"<$prefix$label$attributesString$namespacesString>$childrenString</$prefix$label>"
  }

  override def toString: String = render
}

case class XmlLiteral(value: String) extends XmlValue {
  override def toString: String = value
  override def \(name: String): Seq[XmlValue] = throw new RuntimeException("""\ on XmlLiteral""")
  override def \!(name: String): XmlValue = throw new RuntimeException("""\! on XmlLiteral""")
  override def \!? (name: String): Option[XmlValue] = throw new RuntimeException("""\!? on XmlLiteral""")
  override def \@(name: String): XmlAttribute = throw new RuntimeException("""\@ on XmlLiteral""")
  override def \@?(name: String): Option[XmlAttribute] = throw new RuntimeException("""\@? on XmlLiteral""")
}

object XmlLiteral {
  def apply(o: Any) = new XmlLiteral(o.toString)
}

case class XmlAttribute(
  label: String,
  value: String,
  namespace: Option[Namespace] = None
) {
  override def toString: String = namespace match {
    case Some(Namespace(_, Some(prefix), _, _)) => s"""$prefix:$label="$value""""
    case _ => s"""$label="$value""""
  }
}
object XmlAttribute {
  def fromMap(m: Map[String, String]): Seq[XmlAttribute] =
    m.map{case (x,y) => XmlAttribute(x,y)}.toSeq
}

case class Namespace(uri: String, prefix: Option[String] = None, parent: Option[Namespace] = None, visibleInNode: Boolean = true)

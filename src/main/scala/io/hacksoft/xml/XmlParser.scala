package io.hacksoft.xml

object XmlParser {
  trait ParsedElement // TODO: add attributes and namespaces
  case class OpenTag(label: String, metadata: List[(String, String)] = Nil) extends ParsedElement
  case class CloseTag(label: String) extends ParsedElement
  case class LiteralTag(value: String) extends ParsedElement

  def parse(xmlString: String): XmlValue = {
    // separate all elements to a list
    // classify elements: start/end/startAndEnd
    // TODO: validate elements are opened and closed in the proper way
    // build the actual xml

    val elements = getElementsList(xmlString)
    val parsedElements = getParsedElements(elements)


    getXmlValue(parsedElements, Nil)
  }

  private[xml] def separateChildren(el: List[ParsedElement]): List[List[ParsedElement]] =
    el match {
      case Nil => Nil
      case (lit: LiteralTag) :: tail => List(lit) :: separateChildren(tail)
      case (open: OpenTag) :: tail =>
        val first = open :: tail.takeWhile {
          case CloseTag(label) if label == open.label => false
          case _ => true
        }
        val rest = tail.dropWhile {
          case CloseTag(label) if label == open.label => false
          case _ => true
        } // drop the closing tag itself

        (first :+ rest.head) :: separateChildren(rest.drop(1))
      case _ => throw new Exception("cannot recognise pattern")
    }

  // private to the package so it can be tested
  private[xml] def getXmlValue(parsedElements: List[ParsedElement], parentNamespaces: List[Namespace]): XmlValue = {
    parsedElements match {
      case Nil => XmlLiteral("")
      case List(LiteralTag(value)) => XmlLiteral(value)
      case OpenTag(label, metadata) :: tail if tail.last == CloseTag(`label`) =>
        val realLabel = if (label.contains(':')) {
          label.dropWhile(_ != ':').drop(1)
        } else {
          label
        }
        val labelPrefix = if (label.contains(':')) {
          Some(label.takeWhile(_ != ':'))
        } else {
          None
        }
        val (attrsStrings, nsStrings) = metadata.partition {
          case (k, _) if !k.startsWith("xmlns") => true
          case _ => false
        }
        val attributes = attrsStrings.map { case (k, v) => XmlAttribute(k, v) }
        val namespaces = nsStrings.map { case (k, v) =>
          val prefix = k.dropWhile(_ != ':').drop(1)
          Namespace(
            v,
            if (prefix.isEmpty) None else Some(prefix)
          )
        }

        // Filter out parent namespaces and add them at the end
        val newNamespaces = namespaces.filter (!parentNamespaces.contains (_))

        val (first, rest) = newNamespaces.partition(x => x.prefix == labelPrefix)

        val nsOrderedList = parentNamespaces ++ rest ++ first
        // TODO: extract namespace calculating into a method
        val namespace = {
          val res = nsOrderedList.headOption.map{ x =>
            nsOrderedList.tail.foldLeft(x){(res, cur) => cur.copy(parent = Some(res))}
          }

          // Corner case - if the element has a prefix and the namespace doesn't
          if(labelPrefix.isDefined && res.isDefined && res.get.prefix.isEmpty) {
            Some(Namespace("", labelPrefix, res, false))
          } else res
        }

        val children = separateChildren(tail.init)
          .map(x => getXmlValue(x, nsOrderedList))
          .filter { // remove empty objects from children
            case XmlLiteral("") => false
            case _ => true
          }

        XmlObject(realLabel, children, attributes, namespace)
      case _ => throw new Exception(parsedElements + " cannot be parsed to xml")
    }
  }

  private[xml] def getAttributes(s: String): List[(String, String)] = {
    var res = new StringBuilder()
    var result: List[(String, String)] = Nil
    var attrName = ""
    var lastSymbol = ' '
    var isAString = false
    for(sym <- s) {
      if(sym == '"' && lastSymbol != '\\')
        isAString = !isAString
      else if(sym == '=' && !isAString) {
        attrName = res.toString
        res = new StringBuilder()
      } else if (sym == ' ' && !isAString) {
        result = result :+ (attrName, res.toString)
        res = new StringBuilder()
      } else {
        res += sym
        lastSymbol = sym
      }
    }
    if(attrName.isEmpty)
      result
    else
      result :+ (attrName, res.toString)
  }

  private[xml] def getParsedElements(elements: List[String]): List[ParsedElement] =
    elements.map{ x =>
      if(x.startsWith("</")) CloseTag(x.drop(2).init)
      else if(x.startsWith("<")) {
        val data = x.drop(1).init
        val label = data.takeWhile(_ != ' ')
        val metadata = data.dropWhile(_ != ' ')
        OpenTag(label, getAttributes(metadata.trim))
      }
      else LiteralTag(x)
    }

  private[xml] def getElementsList(s: String): List[String] = {
    var result: List[String] = Nil
    var res = new StringBuilder()
    var inAString = false
    for(sym <- s) {
      if(sym == '"' && res.last != '\\') inAString = !inAString
      if(sym == '<' && !inAString) {
        result = result :+ res.toString
        res = new StringBuilder()
      }
      res += sym
      if(sym == '>' && !inAString) {
        result = result :+ res.toString
        res = new StringBuilder()
      }
    }
    result.drop(1)
  }
}

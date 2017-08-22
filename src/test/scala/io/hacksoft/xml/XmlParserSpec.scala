package io.hacksoft.xml

import io.hacksoft.xml.XmlParser.{CloseTag, LiteralTag, OpenTag}
import org.specs2.mutable.Specification

class XmlParserSpec extends Specification {
  "XmlParser" should {
    "parse simple string" in {
      val xml = "<a><b>text</b></a>"
      val xmlValue = XmlObject("a", Seq(XmlObject("b", Seq(XmlLiteral("text")))))
      XmlParser.parse(xml) mustEqual xmlValue
    }

    "parse attributes" in {
      val xml = """<a foo="bar">text</a>"""
      val xmlValue = XmlObject("a", Seq(XmlLiteral("text")), Seq(XmlAttribute("foo", "bar")))

      XmlParser.parse(xml) mustEqual xmlValue
    }

    "parse namespaces" in {
      val xml = """<foo:a xmlns:foo="http://w.e">text</foo:a>"""
      val xmlValue = XmlObject(
        "a",
        Seq(XmlLiteral("text")),
        Seq(),
        Some(Namespace("http://w.e", Some("foo")))
      )

      XmlParser.parse(xml) mustEqual xmlValue
    }
  }

  "getElementsList" should {
    "work" in {
      val xml = "<a><b>text</b>text</a>"
      val res = List("<a>", "", "<b>", "text", "</b>", "text", "</a>")
      XmlParser.getElementsList(xml) mustEqual res
    }

    "work with < > in a string" in {
      val xml = """<a><b foo="afds<bar>asdf">text</b></a>"""
      val res = List("<a>", "", """<b foo="afds<bar>asdf">""", "text", "</b>", "", "</a>")
      XmlParser.getElementsList(xml) mustEqual res
    }

    """work with \" and < >  in a string""" in {
      val xml = """<a><b foo="afds\"<bar>asdf">text</b></a>"""
      val res = List("<a>", "", """<b foo="afds\"<bar>asdf">""", "text", "</b>", "", "</a>")
      XmlParser.getElementsList(xml) mustEqual res
    }
  }

  "getParsedElements" should {
    "work" in {
      val xml = "<a><b>text</b>text</a>"
      val res = List(OpenTag("a"), LiteralTag(""), OpenTag("b"), LiteralTag("text"), CloseTag("b"), LiteralTag("text"), CloseTag("a"))
      val elementsList = XmlParser.getElementsList(xml)
      XmlParser.getParsedElements(elementsList) mustEqual res
    }

    "work with < > in a string" in {
      val xml = """<a><b foo="afds<bar>asdf">text</b></a>"""
      val res = List(
        OpenTag("a"),
        LiteralTag(""),
        OpenTag("b", List("foo" -> """afds<bar>asdf""")),
        LiteralTag("text"),
        CloseTag("b"),
        LiteralTag(""),
        CloseTag("a")
      )
      val elementsList = XmlParser.getElementsList(xml)
      XmlParser.getParsedElements(elementsList) mustEqual res
    }

    """work with \" and < >  in a string""" in {
      val xml = """<a><b foo="afds\"<bar>asdf">text</b></a>"""
      val res = List(
        OpenTag("a"),
        LiteralTag(""),
        OpenTag("b", List("foo"-> """afds\"<bar>asdf""")),
        LiteralTag("text"),
        CloseTag("b"),
        LiteralTag(""),
        CloseTag("a")
      )
      val elementsList = XmlParser.getElementsList(xml)
      XmlParser.getParsedElements(elementsList) mustEqual res
    }
  }

  "getAttributes" should {
    "work" in {
      val s = """foo="bar" baz="bar""""
      XmlParser.getAttributes(s) mustEqual List(
        "foo" -> "bar",
        "baz" -> "bar"
      )
    }

    """work with a \" """ in {
      val s = """foo="afds\"<bar>asdf" bar="asz""""
      XmlParser.getAttributes(s) mustEqual List(
        "foo" -> """afds\"<bar>asdf""",
        "bar" -> "asz"
      )
    }
  }
}

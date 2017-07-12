package io.hacksoft.xml

import org.specs2.mutable.Specification

class XmlReadsSpec extends Specification {
  "XmlReads" should {

    "read Int" in {
      XmlLiteral("12").as[Int] mustEqual XmlSuccess(12)
      XmlLiteral("").as[Int] must throwA[IllegalArgumentException]
    }

    "read String" in {
      XmlLiteral("test").as[String] mustEqual XmlSuccess("test")
    }

    "read Boolean" in {
      XmlLiteral("true").as[Boolean] mustEqual XmlSuccess(true)
      XmlLiteral("false").as[Boolean] mustEqual XmlSuccess(false)
      XmlLiteral("").as[Boolean] must throwA[IllegalArgumentException]
      XmlLiteral("test").as[Boolean] must throwA[IllegalArgumentException]
    }

    "read List[String]" in {
      val xmlValue = XmlObject("foo", children = Seq(
        XmlLiteral("bar"),
        XmlLiteral("bar"),
        XmlLiteral("bar"),
        XmlLiteral("bar")
      ))

      xmlValue.as[List[String]] mustEqual XmlSuccess(List(
        "bar","bar","bar","bar"
      ))

      XmlObject("test").as[List[String]] mustEqual XmlSuccess(Nil)
    }

    "read List[T]" in {
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar", children = Seq(XmlLiteral("baz"))),
        XmlObject("bar", children = Seq(XmlLiteral("baz"))),
        XmlObject("bar", children = Seq(XmlLiteral("baz")))
      ))

      case class Bar(baz: String)

      implicit val f: XmlReads[Bar] = XmlReads[Bar] {
        case XmlObject("bar", Seq(XmlLiteral(x)), _, _, _) => XmlSuccess(Bar(x))
        case _ => XmlError("Cannot be parsed to a Bar")
      }

      xmlValue.as[List[Bar]] mustEqual XmlSuccess(List(
        Bar("baz"),
        Bar("baz"),
        Bar("baz")
      ))
    }

    "reading List[T] throws correct error" in {
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar", children = Seq(XmlLiteral("baz"))),
        XmlObject("badasd", children = Seq(XmlLiteral("baz"))),
        XmlObject("bar", children = Seq(XmlLiteral("baz")))
      ))

      case class Bar(baz: String)

      implicit val f: XmlReads[Bar] = XmlReads[Bar] {
        case XmlObject("bar", Seq(XmlLiteral(x)), _, _, _) => XmlSuccess(Bar(x))
        case _ => XmlError("Cannot be parsed to a Bar")
      }

      xmlValue.as[List[Bar]] must throwA[RuntimeException]
    }

  }
}

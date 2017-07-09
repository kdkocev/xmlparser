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

    "read List[T]" in {
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

  }
}

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
        x => XmlSuccess(Bar((x \! "bar").as[String].get))
      }

      xmlValue.as[List[Bar]] must throwA[RuntimeException]
    }

    "read T that has implicit XmlReads" in {
      case class User(id: Int, name: String, isConfirmed: Boolean)
      object User {
        implicit val reads: XmlReads[User] = { x =>
          XmlSuccess(User(
            (x \! "id").as[Int].get,
            (x \! "name").as[String].get,
            (x \! "isConfirmed").as[Boolean].get
          ))
        }
      }

      XmlObject("user", Seq(
        XmlObject("id", Seq(XmlLiteral(12))),
        XmlObject("name", Seq(XmlLiteral("John"))),
        XmlObject("isConfirmed", Seq(XmlLiteral("true")))
      )).as[User] mustEqual XmlSuccess(User(12, "John", true))
    }

    "read Option[String]" in {
      XmlLiteral("Thing").as[Option[String]] mustEqual XmlSuccess(Some("Thing"))
    }

    "read Option[T] in complex object" in {
      case class User(id: Option[Int], name: String)
      object User {
        implicit val reads: XmlReads[User] = x => XmlSuccess(User(x.as[Option[Int]].get, x.as[String].get))
      }

      XmlObject("user", Seq(
        XmlObject("id", Seq(XmlLiteral(12))),
        XmlObject("name", Seq(XmlLiteral("John")))
      )).as[User] mustEqual XmlSuccess(User(Some(12), "John"))

      XmlObject("user", Seq(
        XmlObject("name", Seq(XmlLiteral("John")))
      )).as[User] mustEqual XmlSuccess(User(None, "John"))
    }

  }
}

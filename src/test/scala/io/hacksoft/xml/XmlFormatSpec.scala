package io.hacksoft.xml

import org.specs2.mutable.Specification

class XmlFormatSpec extends Specification {

  case class User(id: Int, name: String)
  object User {
    implicit val fmt: XmlFormat[User] = XmlFormat(
      xmlValue => {
        val id = (xmlValue \! "id").as[Int].get
        val name = (xmlValue \! "name").as[String].get
        XmlSuccess(User(id, name))
      },
      user => XmlObject("user", Seq(
        XmlObject("id", Seq(XmlLiteral(user.id))),
        XmlObject("name", Seq(XmlLiteral(user.name)))
      ))
    )
  }


  "Xml format" should {
    "write a simple class" in {
      val o = User(123, "Jeff")
      val xml = "<user><id>123</id><name>Jeff</name></user>"

      XML.write(o).toString mustEqual xml
    }

    "read a simple class" in {
      val o = User(123, "Jeff")
      val xml = "<user><id>123</id><name>Jeff</name></user>"

      XML.read[User](XmlParser.parse(xml)) mustEqual XmlSuccess(o)
    }

    "be implicitly created when a class has reads and writes" in  {
      case class Ob(id: Int)
      object Ob {
        implicit val writes: XmlWrites[Ob] = o => XmlObject("ob", Seq(XmlObject("id", Seq(XmlLiteral(o.id)))))
        implicit val reads: XmlReads[Ob] = xml => XmlSuccess(Ob((xml \! "id").as[Int].get))
      }

      def test (implicit fmt: XmlFormat[Ob]): Int = 1

      test mustEqual 1
    }
  }
}

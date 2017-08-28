package io.hacksoft.xml

import org.specs2.mutable.Specification

class XmlWritesSpec extends Specification {
  "XmlWrites" should {
    def toXml[T](o: T)(implicit w: XmlWrites[T], s: XmlSettings[T] = XmlSettings.default): XmlValue = w.writes(o)

    "write Boolean" in {
      val o = true
      toXml(o) mustEqual XmlLiteral(o)

      val o2 = false
      toXml(o2) mustEqual XmlLiteral(o2)
    }
    "write Int" in {
      val o = 12
      toXml(o) mustEqual XmlLiteral(o)
    }
    "write String" in {
      val o = "test"
      toXml(o) mustEqual XmlLiteral(o)
    }
    "write Char" in {
      val o = 'a'
      toXml(o) mustEqual XmlLiteral(o)
    }
    "write Double" in {
      val o = 5.5d
      toXml(o) mustEqual XmlLiteral(o)
    }
    "write Foat" in {
      val o = 5.2f
      toXml(o) mustEqual XmlLiteral(o)
    }

    "write List[String]" in {
      case class Word(word: String)
      implicit val writes: XmlWrites[Word] = x => XmlObject("listItem", Seq(XmlLiteral(x.word)))

      implicit val settings: XmlSettings[Word] = XmlSettings(listName = "customListName")

      val o = List(Word("Hello"), Word("World"))


      toXml(o) mustEqual XmlObject("customListName", Seq(
        XmlObject("listItem", Seq(XmlLiteral("Hello"))),
        XmlObject("listItem", Seq(XmlLiteral("World")))
      ))
    }

    "write List[T]" in {
      case class User(id: Int, name: String)
      object User {
        implicit val writes: XmlWrites[User] = {x =>
          XmlObject("user", Seq(
            XmlObject("id", Seq(XmlLiteral(x.id))),
            XmlObject("name", Seq(XmlLiteral(x.name)))
          ))
        }
        implicit val settings: XmlSettings[User] = XmlSettings(listName = "users")
      }

      val o = List(
        User(12, "John"),
        User(13, "Jay")
      )

      toXml(o) mustEqual XmlObject("users", Seq(
        XmlObject("user", Seq(
          XmlObject("id", Seq(XmlLiteral(12))),
          XmlObject("name", Seq(XmlLiteral("John")))
        )),
        XmlObject("user", Seq(
          XmlObject("id", Seq(XmlLiteral(13))),
          XmlObject("name", Seq(XmlLiteral("Jay")))
        ))
      ))
    }

    "write List[T] wrapped in class" in {
      case class Game(name: String)
      case class Games(games: List[Game])
      object Game {
        implicit val writes: XmlWrites[Game] = x => XmlObject("game", Seq(XmlLiteral(x.name)))
      }
      object Games {
        implicit val writes: XmlWrites[Games] = x => XmlObject("games", x.games.map(toXml(_)))
      }

      val o = Games(List(Game("A"), Game("B")))
      toXml(o) mustEqual XmlObject("games", Seq(
        XmlObject("game", Seq(XmlLiteral("A"))),
        XmlObject("game", Seq(XmlLiteral("B")))
      ))
    }

    // TODO: add option writes
  }

  "XmlWrites with XmlParser" should {
    "write a Word() to xml string" in {
      case class Word(word: String)
      implicit val writes: XmlWrites[Word] = x => XmlObject("word", Seq(XmlLiteral(x.word)))

      val obj = Word("Hello!")
      writes.writes(obj).toString mustEqual "<word>Hello!</word>"
    }
  }
}

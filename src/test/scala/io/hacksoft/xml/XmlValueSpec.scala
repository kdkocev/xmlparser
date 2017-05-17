package io.hacksoft.xml

import java.net.URL

import org.specs2.mutable.Specification

import scala.xml._

class XmlValueSpec extends Specification {

  "XmlValue" should {
    "literal" in {
      val theData = "some literal value"
      val xmlElem = new Text(theData)
      val xmlValue = XmlLiteral(theData)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Atom[T]" in {
      val theData = 123
      val xmlElem = new Atom(theData)
      val xmlValue = XmlLiteral(theData.toString)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty Elem" in {
      val xmlElem = <foo></foo>
      val xmlValue = XmlObject("foo")

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty minimised Elem" in {
      val xmlElem = <foo/>
      val xmlValue = XmlObject("foo", minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty Elem with one attribute" in {
      val xmlElem = <foo bar="text"></foo>
      val xmlValue = XmlObject("foo", attributes = Seq(XmlAttribute("bar","text")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty Elem with attributes" in {
      val xmlElem = <foo bar1="text1" bar2="text2"></foo>
      val xmlValue = XmlObject("foo", attributes = Seq(XmlAttribute("bar1","text1"), XmlAttribute("bar2","text2")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty minimised Elem with one attribute" in {
      val xmlElem = <foo bar="text"/>
      val xmlValue = XmlObject("foo", attributes = Seq(XmlAttribute("bar", "text")), minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "empty minimised Elem with attributes" in {
      val xmlElem = <foo bar1="text1" bar2="text2"/>
      val attribs = Seq(XmlAttribute("bar1","text1"), XmlAttribute("bar2","text2"))
      val xmlValue = XmlObject("foo", attributes = attribs, minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with one atomic child" in {
      val xmlElem = <foo>bar</foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlLiteral("bar")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with atomic children" in {
      val xmlElem =
        <foo>
          bar
          bar
          bar
        </foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlLiteral("bar"), XmlLiteral("bar"), XmlLiteral("bar")))

      Utility.trim(xmlElem).toString mustEqual xmlValue.toString
    }

    "Elem minimised with one atomic child" in {
      val xmlElem = <foo>bar</foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlLiteral("bar")), minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem minimised with atomic children" in {
      val xmlElem = <foo>bar bar bar</foo>
      val children = Seq(XmlLiteral("bar"), XmlLiteral("bar"), XmlLiteral("bar"))
      val xmlValue = XmlObject("foo", children = children, minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with atomic children and attributes" in {
      val xmlElem = <foo bar1="text1" bar2="text2">text3 text4</foo>
      val attribs = XmlAttribute.fromMap(Map("bar1" -> "text1", "bar2" -> "text2"))
      val children = Seq(XmlLiteral("text3"), XmlLiteral("text4"))
      val xmlValue = XmlObject("foo", children = children, attributes = attribs)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with atomic children and attributes minimised" in {
      val xmlElem = <foo bar1="text1" bar2="text2">text3 text4</foo>
      val attribs = XmlAttribute.fromMap(Map("bar1" -> "text1", "bar2" -> "text2"))
      val children = Seq(XmlLiteral("text3"), XmlLiteral("text4"))
      val xmlValue = XmlObject("foo", children = children, attributes = attribs, minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace"

    "Elem with namespace and attributes"

    "Elem with namespace and namespaced attributes"

    "Elem with one empty complex child" in {
      val xmlElem = <foo><bar></bar></foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlObject("bar")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with empty complex children" in {
      val xmlElem = <foo><bar1></bar1><bar2></bar2></foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlObject("bar1"), XmlObject("bar2")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with non-empty complex child" in {
      val xmlElem = <foo><bar>text</bar></foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlObject("bar", children = Seq(XmlLiteral("text")))))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with non-empty complex children" in {
      val xmlElem = <foo><bar1>text1</bar1><bar2>text2</bar2></foo>
      val xmlValue =
        XmlObject("foo", children =
          Seq(
            XmlObject("bar1", children = Seq(
              XmlLiteral("text1"))),
            XmlObject("bar2", children = Seq(
              XmlLiteral("text2")
            ))
          )
        )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with one non-empty complex child and one atomic child" in {
      val xmlElem = <foo><bar>text1</bar>text2</foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar", children = Seq(XmlLiteral("text1"))),
        XmlLiteral("text2")
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with complex and atomic children" in {
      val xmlElem = <foo>text1<bar1>text2</bar1><bar2>text3</bar2>text4 text5 text6<bar3>text7</bar3></foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlLiteral("text1"),
        XmlObject("bar1", children = Seq(XmlLiteral("text2"))),
        XmlObject("bar2", children = Seq(XmlLiteral("text3"))),
        XmlLiteral("text4"),
        XmlLiteral("text5"),
        XmlLiteral("text6"),
        XmlObject("bar3", children = Seq(XmlLiteral("text7")))
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced child"

    "Elem with namespace and namespaced complex children"

    "Elem with namespace and namespaced complex children and namespaced attributes"

    "Elem with same namespace on all children"

    "Elem with same namespace on it and all children"

    "Elem with two different namespaces for all children and all attributes"

    "Elem with equal names on attrbute and child" in {
      val xmlElem = <foo bar="text"><bar>text</bar></foo>
      val xmlValue = XmlObject("foo",
        children = Seq(XmlObject("bar", children = Seq(XmlLiteral("text")))),
        attributes = Seq(XmlAttribute("bar", "text"))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with the same names on all children" in {
      val xmlElem = <foo><bar></bar><bar></bar><bar></bar></foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar"),
        XmlObject("bar"),
        XmlObject("bar")
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with the same names on all children except one" in {
      val xmlElem = <foo><bar></bar><bar></bar><bar></bar><bar1></bar1></foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar"),
        XmlObject("bar"),
        XmlObject("bar"),
        XmlObject("bar1")
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with the same names on all children except two" in {
      val xmlElem = <foo><bar></bar><bar></bar><bar></bar><bar1></bar1><bar2></bar2></foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar"),
        XmlObject("bar"),
        XmlObject("bar"),
        XmlObject("bar1"),
        XmlObject("bar2")
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with minimised complex child" in {
      val xmlElem = <foo><bar/></foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlObject("bar", minimiseEmpty = true)))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with non-minimised empty complex child" in {
      val xmlElem = <foo><bar></bar></foo>
      val xmlValue = XmlObject("foo", children = Seq(XmlObject("bar")))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with one minimised empty child and one non-minimised empty child" in {
      val xmlElem = <foo><bar1/><bar2></bar2></foo>
      val xmlValue = XmlObject("foo", children = Seq(
        XmlObject("bar1", minimiseEmpty = true),
        XmlObject("bar2")
      ))

      xmlElem.toString mustEqual xmlValue.toString
    }
  }
}

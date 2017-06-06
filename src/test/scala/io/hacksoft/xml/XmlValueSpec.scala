package io.hacksoft.xml

import org.specs2.mutable.Specification

import scala.xml._

class XmlValueSpec extends Specification {

  object TestNS {
    val ns1 = "http://google.com"
    val ns2 = "http://www.w3.org/1999/xhtml"
    val ns3 = "https://www.w3.org/TR/xhtml11/"
    val ns4 = "http://www.w3.org/2001/XMLSchema-instance"
  }

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

    "Elem with general namespace" in {
      val xmlElem = <foo xmlns={TestNS.ns1}></foo>
      val xmlValue = XmlObject("foo", namespace = Some(Namespace(TestNS.ns1)))

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with prefixed namespace" in {
      val xmlElem = <foo xmlns:i={TestNS.ns1}></foo>
      val xmlValue = XmlObject("foo",
        namespace = Some(Namespace("", prefix = None, visibleInNode = false, parent =
          Some(Namespace(TestNS.ns1, prefix = Some("i")))
        ))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem empty, with namespace, minimised" in {
      val xmlElem = <foo xmlns={TestNS.ns1}/>
      val xmlValue = XmlObject("foo", namespace = Some(Namespace(TestNS.ns1)), minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem empty, with namespace, with prefix and minimised" in {
      val xmlElem = <a:foo xmlns:a={TestNS.ns1} />
      val xmlValue = XmlObject("foo", namespace = Some(Namespace(TestNS.ns1, prefix = Some("a"))), minimiseEmpty = true)

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and non-namespaced attributes" in {
      val xmlElem = <a:foo bar1="text" bar2="text" xmlns:a={TestNS.ns1}></a:foo>
      val xmlValue = XmlObject("foo",
        attributes = Seq(XmlAttribute("bar1","text"),XmlAttribute("bar2","text")),
        namespace = Some(Namespace(TestNS.ns1, prefix = Some("a")))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and attributes with the same namespace" in {
      val xmlElem = <foo bar1="text" bar2="text2" xmlns={TestNS.ns1}></foo>
      val xmlValue = XmlObject("foo",
        attributes = Seq(
          XmlAttribute("bar1","text", namespace = Some(Namespace(TestNS.ns1))),
          XmlAttribute("bar2", "text2", namespace = Some(Namespace(TestNS.ns1)))
        ),
        namespace = Some(Namespace(TestNS.ns1))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and attributes with the same namespace and child with no namespace" in {
      val xmlElem = <a:foo a:bar="text" xmlns:a={TestNS.ns1} ><bar></bar></a:foo>
      val xmlValue = XmlObject("foo",
        children = Seq(XmlObject("bar")),
        attributes = Seq(XmlAttribute("bar","text", Some(Namespace(TestNS.ns1, prefix = Some("a"))))),
        namespace = Some(Namespace(TestNS.ns1, prefix = Some("a")))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced attributes" in {
      val xmlElem = <foo a:bar1="text" b:bar2="text" xmlns:a={TestNS.ns2} xmlns:b={TestNS.ns3} xmlns={TestNS.ns1} ></foo>
      val xmlValue = XmlObject("foo",
        attributes = Seq(
          XmlAttribute("bar1", "text", Some(Namespace(TestNS.ns2, prefix = Some("a")))),
          XmlAttribute("bar2", "text", Some(Namespace(TestNS.ns3, prefix = Some("b"))))
        ),
        namespace = Some(Namespace(TestNS.ns1))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced attributes with same names" in {
      val xmlElem = <foo a:bar="text1" b:bar="text2" xmlns:a={TestNS.ns2} xmlns:b={TestNS.ns3} xmlns={TestNS.ns1} ></foo>
      val xmlValue = XmlObject("foo",
        attributes = Seq(
          XmlAttribute("bar", "text1", Some(Namespace(TestNS.ns2, prefix = Some("a")))),
          XmlAttribute("bar", "text2", Some(Namespace(TestNS.ns3, prefix = Some("b"))))
        ),
        namespace = Some(Namespace(TestNS.ns1))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

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

    "Elem with namespace and namespaced child" in {
      val xmlElem = <foo xmlns={TestNS.ns1}><bar></bar></foo>
      val xmlValue = XmlObject("foo",
        children = Seq(XmlObject("bar", namespace = Some(Namespace(TestNS.ns1, visibleInNode = false)))),
        namespace = Some(Namespace(TestNS.ns1))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced child with prefix" in {
      val xmlElem = <a:foo xmlns:a={TestNS.ns1}><a:bar></a:bar></a:foo>
      val xmlValue = XmlObject("foo",
        children = Seq(XmlObject("bar",
          namespace = Some(Namespace(TestNS.ns1, prefix = Some("a"), visibleInNode = false))
        )),
        namespace = Some(Namespace(TestNS.ns1, prefix = Some("a")))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced complex children" in {
      val xmlElem = <foo xmlns={TestNS.ns1}><bar xmlns={TestNS.ns2}></bar></foo>
      val xmlValue = XmlObject("foo",
        children = Seq(XmlObject("bar", namespace = Some(Namespace(TestNS.ns2)))),
        namespace = Some(Namespace(TestNS.ns1))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced complex children with prefix" in {
      val xmlElem = <a:foo xmlns:a={TestNS.ns1} xmlns:b={TestNS.ns2}><b:bar></b:bar><b:bar></b:bar></a:foo>
      val xmlValue = XmlObject("foo",
        children = Seq(
          XmlObject("bar", namespace = Some(Namespace(TestNS.ns2, prefix=Some("b"), visibleInNode = false))),
          XmlObject("bar", namespace = Some(Namespace(TestNS.ns2, prefix=Some("b"), visibleInNode = false)))
        ),
        namespace = Some(Namespace(TestNS.ns1,
          prefix = Some("a"),
          parent = Some(Namespace(TestNS.ns2, prefix=Some("b")))
        ))
      )

      xmlElem.toString mustEqual xmlValue.toString
    }

    "Elem with namespace and namespaced complex children and namespaced attributes" in {
      val xmlElem =
        <a:foo c:bar1="test" xmlns:c={TestNS.ns3} xmlns:a={TestNS.ns1} xmlns:i={TestNS.ns4} xmlns:b={TestNS.ns2}>
          <b:bar i:nil="true"></b:bar>
          <b:bar></b:bar>
        </a:foo>
      val xmlValue = XmlObject("foo",
        children = Seq(
          XmlObject("bar",
            attributes = Seq(XmlAttribute("nil", "true", namespace = Some(Namespace(TestNS.ns4, prefix = Some("i"), visibleInNode = false)))),
            namespace = Some(Namespace(TestNS.ns2, prefix=Some("b"), visibleInNode = false)),
            minimiseEmpty = true
          ),
          XmlObject("bar", namespace = Some(Namespace(TestNS.ns2, prefix=Some("b"), visibleInNode = false)), minimiseEmpty = true)
        ),
        attributes = Seq(
          XmlAttribute("bar1", "test", namespace = Some(Namespace(TestNS.ns3, prefix = Some("c"))))
        ),
        namespace = Some(Namespace(TestNS.ns1,
          prefix = Some("a"),
          parent = Some(Namespace(TestNS.ns2, prefix=Some("b"), parent = Some(
            Namespace(TestNS.ns3, prefix = Some("c"), parent = Some(
              Namespace(TestNS.ns4, prefix = Some("i"))
            ))
          )))
        ))
      )

      Utility.trim(xmlElem).toString mustEqual xmlValue.toString
    }

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

    "Elem with deep nested complex children" in {
      val xmlElem =
        <foo>
          <bar1>
            <bar11>
              <bar111>barbar</bar111>
            </bar11>
          </bar1>
          <bar2>
            <bar21>
              <bar211>barbarbar</bar211>
            </bar21>
            <bar22>
              <bar221>barbarbarbar</bar221>
            </bar22>
          </bar2>
        </foo>
      val xmlValue =
        XmlObject("foo", children = Seq(
          XmlObject("bar1", children = Seq(
            XmlObject("bar11", children = Seq(
              XmlObject("bar111", children = Seq(
                XmlLiteral("barbar")
              ))
            ))
          )),
          XmlObject("bar2", children = Seq(
            XmlObject("bar21", children = Seq(
              XmlObject("bar211", children = Seq(
                XmlLiteral("barbarbar")
              ))
            )),
            XmlObject("bar22", children = Seq(
              XmlObject("bar221", children = Seq(
                XmlLiteral("barbarbarbar")
              ))
            ))
          ))
        ))

      Utility.trim(xmlElem).toString mustEqual xmlValue.toString
    }
  }
}

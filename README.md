# Xml Parser

A project for parsing and rendering objects from and to XML

## How to use:

### Renderer

1. Define an implicit way to render your type in the companion object
```scala
  case class Word(word: String)
  object Word {
    implicit val writes: XmlWrites[Word] = x => XmlObject("word", Seq(XmlLiteral(x.word)))
  }
```

2. Use
```scala
  val obj = Word("Hello!")
  
  XML.write(obj).toString // mustEqual "<word>Hello!</word>"
```

**Note:** Also works with complex and nested types that you defined `XmlWrites` for

### Parser

1. Define an implicit way to read your type from string

```scala
  case class User(id: Int, name: String)
  object User {
    implicit val reads: XmlReads[User] = x =>
      XmlSuccess(
        User(
          id = (x \! "id").as[Int].get,
          name = (x \! "name").as[String].get
         )
      )
  }
```

**Note**: This is not the best example but it is simplified for demo purposes

2. Use

```scala
    val xml = "<user><id>12</id><name>John</name></user>"
    val result = User(Some(12), "John")
    
    XmlParser.parse(xml).as[User] // mustEqual XmlSuccess(result)
```

### Format

Format is a more concise way to define XmlReads and XmlWrites at the same time

```scala
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

    ...

    val o = User(123, "Jeff")
    val xml = "<user><id>123</id><name>Jeff</name></user>"
    
    XML.write(o).toString // mustEqual xml
    XML.read[User](XmlParser.parse(xml)) // mustEqual XmlSuccess(o)
```


For examples, please check the `*Spec.scala` files

### TODO:

 * Make a macro for auto xml rendering on ADTs
 * Recognise `XmlObject` definitions from `Map[String -> Map[...]]`
 * A way to define implicit `XmlReads` easier
 * Add more examples to the Readme
 * Code cleanup

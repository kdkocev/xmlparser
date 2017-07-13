package io.hacksoft.xml

/**
  * This feature is experimental
  */

case class XmlSettings[+T](
  // Only used when writing List[T] that is not wrapped in a class
  listName: String = "list"
)

object XmlSettings {
  def default: XmlSettings[Nothing] = XmlSettings[Nothing]()
}

/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package toolxit.bibtex
package tree

import java.net.URL

// a raw entry as returned by the parser before it is refined for later use
sealed trait Raw

final case class BibTeXDatabase(entries: List[Entry]) extends Raw

sealed trait Entry extends Raw

final case class StringEntry(name: String, value: Value) extends Entry

sealed trait Value extends Ordered[Value]
final case class StringValue(value: String) extends Value {
  def compare(that: Value) = that match {
    case StringValue(s) => value.compare(s)
    case c: ConcatValue =>
      value.compare(c.resolved)
    case _ => 1
  }

  override def toString = "\"" + value + "\""
}
final case class IntValue(value: Int) extends Value {
  def compare(that: Value) = that match {
    case IntValue(i) => value.compare(i)
    case EmptyValue => 1
    case _ => -1
  }

  override def toString = value.toString
}
final case class ConcatValue(parts: List[Value]) extends Value {
  var resolved = ""
  def compare(that: Value) = that match {
    case StringValue(s) => resolved.compare(s)
    case c: ConcatValue =>
      resolved.compare(c.resolved)
    case _ => 1
  }

  override def toString = parts.mkString(" # ")
}
final case class NameValue(name: String) extends Value {
  var resolved = ""
  def compare(that: Value) = that match {
    case StringValue(s) => resolved.compare(s)
    case c: ConcatValue =>
      resolved.compare(c.resolved)
    case _ => 1
  }

  override def toString = name
}
case object EmptyValue extends Value {
  def compare(that: Value) = that match {
    case EmptyValue => 0
    case _ => -1
  }

  override def toString = ""
}

final case class PreambleEntry(value: ConcatValue) extends Entry

final case class BibEntry(name: String,
                          key: String,
                          fields: List[Field]) extends Entry {
  var sortKey = key

  def sortValue = fields.find(_.name == sortKey).map(_.value).getOrElse(EmptyValue)

}
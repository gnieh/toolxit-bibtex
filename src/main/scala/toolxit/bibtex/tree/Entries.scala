/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.bibtex
package tree

import java.net.URL
import scala.util.parsing.input.Positional

// a raw entry as returned by the parser before it is refined for later use
sealed trait Raw extends Positional

final case class BibTeXDatabase(entries: List[BibEntry],
                                strings: List[StringEntry],
                                preambles: List[PreambleEntry]) extends Raw {

  /** Finds the BibTeX entry associated to the key, if any */
  def find(key: String) = entries.find {
    case BibEntry(_, k, _) if k == key => true
    case _ => false
  }

}

sealed trait Entry extends Raw

final case class StringEntry(name: String, value: Value) extends Entry

sealed trait Value extends Ordered[Value] {
  /** Returns a resolved value if it uses names */
  def resolve(env: Map[String, String]): String
}
final case class StringValue(value: String) extends Value {
  def compare(that: Value) = that match {
    case StringValue(s) => value.compare(s)
    case c: ConcatValue =>
      value.compare(c.resolved.getOrElse(""))
    case _ => 1
  }

  def resolve(env: Map[String, String]) = value

  override def toString = "\"" + value + "\""
}
final case class IntValue(value: Int) extends Value {
  def compare(that: Value) = that match {
    case IntValue(i) => value.compare(i)
    case EmptyValue => 1
    case _ => -1
  }

  def resolve(env: Map[String, String]) = value.toString

  override def toString = value.toString
}
final case class ConcatValue(parts: List[Value]) extends Value {
  var resolved: Option[String] = None
  def compare(that: Value) = that match {
    case StringValue(s) => resolved.getOrElse("").compare(s)
    case c: ConcatValue =>
      resolved.getOrElse("").compare(c.resolved.getOrElse(""))
    case _ => 1
  }

  def resolve(env: Map[String, String]) = {
    if (resolved.isEmpty)
      resolved = Some(parts.map(_.resolve(env)).mkString)
    resolved.getOrElse("")
  }

  override def toString = resolved match {
    case Some(r) => r
    case _ => parts.mkString(" # ")
  }
}
final case class NameValue(name: String) extends Value {
  var resolved: Option[String] = None
  def compare(that: Value) = that match {
    case StringValue(s) => resolved.getOrElse("").compare(s)
    case c: ConcatValue =>
      resolved.getOrElse("").compare(c.resolved.getOrElse(""))
    case _ => 1
  }

  def resolve(env: Map[String, String]) = {
    if (resolved.isEmpty)
      resolved = env.get(name)
    resolved.getOrElse("")
  }

  override def toString = resolved match {
    case Some(r) => r
    case _ => name
  }
}
case object EmptyValue extends Value {
  def compare(that: Value) = that match {
    case EmptyValue => 0
    case _ => -1
  }

  def resolve(env: Map[String, String]) = ""

  override def toString = ""
}

final case class PreambleEntry(value: ConcatValue) extends Entry

final case class BibEntry(name: String,
                          key: String,
                          fields: List[Field]) extends Entry {
  var sortKey = key

  def sortValue = fields.find(_.name == sortKey).map(_.value).getOrElse(EmptyValue)

  def field(name: String): Option[Value] =
    fields.find(_.name == name).map(_.value)

  def toBibTeX = "@" + name + " {" + key + ",\n" +
    fields.map(_.toBibTeX).mkString("  ", "\n  ", "\n}")

}
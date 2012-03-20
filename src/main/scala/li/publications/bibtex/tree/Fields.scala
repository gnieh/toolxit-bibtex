/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package tree

import java.net.URL

sealed abstract class Field(val name: String)

final case class Address(value: String) extends Field("address")

final case class Annote(value: String) extends Field("annote")

final case class Author(firstname: String,
                        lastname: String,
                        middlename: Option[String]) extends Field("author")

final case class BookTitle(value: String) extends Field("booktitle")

final case class Chapter(value: Int) extends Field("chapter")

final case class CrossRef(key: String) extends Field("crossref")

final case class Edition(value: String) extends Field("edition")

final case class Editor(value: String) extends Field("editor")

final case class Eprint(value: String) extends Field("eprint")

final case class HowPublished(value: String) extends Field("howpublished")

final case class Institution(value: String) extends Field("institution")

final case class Journal(value: String) extends Field("journal")

final case class Key(value: String) extends Field("key")

final case class Month(value: String) extends Field("month")

final case class Note(value: String) extends Field("note")

final case class Number(value: Int) extends Field("number")

final case class Organization(value: String) extends Field("organization")

final case class Pages(value: String) extends Field("pages")

final case class Publisher(value: String) extends Field("publisher")

final case class School(value: String) extends Field("school")

final case class Series(value: String) extends Field("series")

final case class Title(value: String) extends Field("title")

final case class Type(value: String) extends Field("type")

final case class Url(value: URL) extends Field("url")

final case class Volume(value: Int) extends Field("volume")

final case class Year(value: Int) extends Field("year")

final case class UnknownField(override val name: String, value: String) extends Field(name)

// A raw field is a field as returned by the parser before being refined
final case class RawField(override val name: String, value: Value) extends Field(name)
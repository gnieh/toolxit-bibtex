/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package tree

import java.net.URL
import org.kiama.attribution.Attributable

// the entries

sealed abstract class Entry(val key: String) extends Attributable {
  var extraFields: List[Field] = Nil
  var isCommented = false
  def commented_? = isCommented
}

final case class Article(override val key: String,
                         authors: List[Author],
                         title: Title,
                         journal: Journal,
                         year: Year,
                         volume: Option[Volume],
                         number: Option[Number],
                         pages: Option[Pages],
                         month: Option[Month],
                         note: Option[Note],
                         _key: Option[Key]) extends Entry(key)

final case class Book(override val key: String,
                      author_editor: Either[Author, Editor],
                      title: Title,
                      publisher: Publisher,
                      year: Year,
                      volume_number: Option[Either[Volume, Number]],
                      series: Option[Series],
                      address: Option[Address],
                      edition: Option[Edition],
                      month: Option[Month],
                      note: Option[Note],
                      _key: Option[Key]) extends Entry(key)

final case class Booklet(override val key: String,
                         title: Title,
                         author: Option[Author],
                         howpublished: Option[HowPublished],
                         address: Option[Address],
                         month: Option[Month],
                         year: Option[Month],
                         note: Option[Note],
                         _key: Option[Key]) extends Entry(key)

final case class Conference(override val key: String,
                            author: Author,
                            title: Title,
                            booktitle: BookTitle,
                            year: Year,
                            editor: Option[Editor],
                            volume_number: Option[Either[Volume, Number]],
                            series: Option[Series],
                            pages: Option[Pages],
                            address: Option[Address],
                            month: Option[Month],
                            organization: Option[Organization],
                            publisher: Option[Publisher],
                            note: Option[Note],
                            _key: Option[Key]) extends Entry(key)

final case class InBook(override val key: String,
                        author_editor: Either[Author, Editor],
                        title: Title,
                        chapter_pages: Either[Chapter, Pages],
                        publisher: Publisher,
                        year: Year,
                        volume_number: Option[Either[Volume, Number]],
                        series: Option[Series],
                        tpe: Option[Type],
                        address: Option[Address],
                        edition: Option[Edition],
                        month: Option[Month],
                        note: Option[Note],
                        _key: Option[Key]) extends Entry(key)

final case class InCollection(override val key: String,
                              author: Author,
                              title: Title,
                              booktitle: BookTitle,
                              publisher: Publisher,
                              year: Year,
                              editor: Option[Editor],
                              volume_number: Option[Either[Volume, Number]],
                              series: Option[Series],
                              tpe: Option[Type],
                              chapter: Option[Chapter],
                              pages: Option[Pages],
                              address: Option[Address],
                              edition: Option[Edition],
                              month: Option[Month],
                              note: Option[Note],
                              _key: Option[Key]) extends Entry(key)

final case class InProceedings(override val key: String,
                               author: Author,
                               title: Title,
                               booktitle: BookTitle,
                               year: Year,
                               editor: Option[Editor],
                               volume_number: Option[Either[Volume, Number]],
                               series: Option[Series],
                               pages: Option[Pages],
                               address: Option[Address],
                               month: Option[Month],
                               organization: Option[Organization],
                               publisher: Option[Publisher],
                               note: Option[Note],
                               _key: Option[Key]) extends Entry(key)

final case class Manual(override val key: String,
                        title: Title,
                        author: Option[Author],
                        organization: Option[Organization],
                        address: Option[Address],
                        edition: Option[Edition],
                        month: Option[Month],
                        year: Option[Year],
                        note: Option[Note],
                        _key: Option[Key]) extends Entry(key)

final case class MastersThesis(override val key: String,
                               author: Author,
                               title: Title,
                               school: School,
                               year: Year,
                               tpe: Option[Type],
                               address: Option[Address],
                               month: Option[Month],
                               note: Option[Note],
                               _key: Option[Key]) extends Entry(key)

final case class Misc(override val key: String,
                      author: Option[Author],
                      title: Option[Title],
                      howpublished: Option[HowPublished],
                      month: Option[Month],
                      year: Option[Year],
                      note: Option[Note],
                      _key: Option[Key]) extends Entry(key)

final case class PhdThesis(override val key: String,
                           author: Author,
                           title: Title,
                           school: School,
                           year: Year,
                           tpe: Option[Type],
                           address: Option[Address],
                           month: Option[Month],
                           note: Option[Note],
                           _key: Option[Key]) extends Entry(key)

final case class Proceedings(override val key: String,
                             title: Title,
                             year: Year,
                             editor: Option[Editor],
                             volume_number: Option[Either[Volume, Number]],
                             series: Option[Series],
                             address: Option[Address],
                             month: Option[Month],
                             publisher: Option[Publisher]) extends Entry(key)

final case class TechReport(override val key: String,
                            author: Author,
                            title: Title,
                            institution: Institution,
                            year: Year,
                            tpe: Option[Type],
                            number: Option[Number],
                            address: Option[Address],
                            month: Option[Month],
                            note: Option[Note],
                            _key: Option[Key]) extends Entry(key)

final case class Unpublished(override val key: String,
                             author: Author,
                             title: Title,
                             note: Note,
                             month: Option[Month],
                             year: Option[Year],
                             _key: Option[Key]) extends Entry(key)

final case class UnknownEntry(name: String,
                              override val key: String,
                              fields: List[Field]) extends Entry(key) {
  extraFields = fields
}

// a raw entry as returned by the parser before it is refined for later use
final case class BibFile(entries: List[RawEntry]) extends Attributable

sealed trait RawEntry extends Attributable

final case class StringEntry(name: String, value: Value) extends RawEntry

sealed trait Value
final case class StringValue(value: String) extends Value
final case class ConcatValue(parts: List[Value]) extends Value
final case class NameValue(name: String) extends Value

final case class PreambleEntry(value: Value) extends RawEntry

final case class BibEntry(name: String,
                          key: String,
                          fields: List[Field]) extends RawEntry
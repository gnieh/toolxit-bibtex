/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

class Author(val firstname: Option[String],
             val von: Option[String],
             val lastname: String,
             val jr: Option[String]) {

  override def toString =
    "first: " + firstname.getOrElse("") +
      "\nvon: " + von.getOrElse("") +
      "\nlast: " + lastname +
      "\njr: " + jr.getOrElse("")

}

object Author {
  private def option(string: String) =
    if (string.nonEmpty) Some(string) else None

  def apply(first: String, von: String, last: String, jr: String): Author = {
    val realFirst = option(first)
    val realVon = option(von)
    val realJr = option(jr)
    new Author(realFirst, realVon, last, realJr)
  }

  def unapply(author: Author): Option[(Option[String], Option[String], String, Option[String])] = {
    Some((author.firstname, author.von, author.lastname, author.jr))
  }

}

case object EmptyAuthor extends Author(None, None, "", None)
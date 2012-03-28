/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import scala.util.parsing.combinator.RegexParsers

/**
 * This class is a helper to extract the different names of the authors.
 *
 * @author Lucas Satabin
 *
 */
object Authors {

  implicit def char2testable(c: Char) = new {
    def isAlphaNumeric = c.toString.matches("[a-zA-Z0-9]")
    def isBibTeXLower =
      if (c.isDigit)
        true
      else
        c.isLower
  }

  sealed trait StringElement
  final case class Block(parts: List[StringElement]) extends StringElement {
    override def toString = parts.mkString("{", "", "}")
  }
  final case class Word(value: String) extends StringElement {
    override def toString = value
  }
  final case class Sentence(value: String) extends StringElement {
    override def toString = value
  }
  final case class Special(spec: String, char: Option[String]) extends StringElement {
    override def toString = {
      val block = char match {
        case Some(c) => "{" + c + "}"
        case _ => ""
      }
      "\\" + spec + block
    }
  }

  object NameParser extends RegexParsers {

    lazy val author = lastFirstParts | firstLastParts

    lazy val firstLastParts =
      rep(block | special | "\\w+".r ^^ Word) ^^ {
        case l @ some :: tail =>
          // the lastname part contains at least the last word
          var lastname = l.last
          // remaining word, removing at least the last one which is in the lastname part
          val remaining = l.dropRight(1)
          // the von part (if any) is anything between the first lower case 
          // word and the last one in lower case
          val (firstname, von, last) = toFirstVonLast(remaining)
          Author(firstname, von, last + lastname, "")
        case _ => EmptyAuthor
      }

    lazy val lastFirstParts =
      rep2sep(rep(block | special | "\\w+".r ^^ Word), ",")

    lazy val block: Parser[Block] =
      "{" ~> rep("[^\\{}]+".r ^^ Sentence | special | block) <~ "}" ^^ Block

    lazy val special: Parser[Special] =
      "\\" ~> "[^\\s{]+".r ~ opt("{" ~> "[^}]*".r <~ "}") ^^ {
        case spec ~ char => Special(spec, char)
      }

    def rep2sep[T, U](p: => Parser[T], s: => Parser[U]) =
      p ~ rep1(s ~> p) ^^ { case x ~ y => x :: y }

  }

  def toFirstVonLast(parts: List[StringElement]) = {
    var first = ""
    var von = ""
    var last = ""
    var isFirst = true
    var hasVon = false
    parts.foreach { part =>
      if (isFirstCharacterLower(part) && last.nonEmpty) {
        hasVon = true
        isFirst = false
        von = von + " " + last + " " + part
        last = ""
      } else if (isFirstCharacterLower(part)) {
        hasVon = true
        isFirst = false
        von = von + " " + part
      } else if (isFirst) {
        first = first + " " + part
      } else {
        last = last + " " + part
      }
    }
    if (last.nonEmpty)
      last = last + " "
    (first, von, last)
  }

  /* returns the first non brace character at level 0 if any */
  def firstCharacter(str: StringElement): Option[Char] = {
    str match {
      case Word(value) if value.nonEmpty => Some(value(0))
      case Sentence(value) if value.nonEmpty => Some(value(0))
      case Block((s: Special) :: _) =>
        firstCharacter(s)
      case Block((b: Block) :: _) =>
        firstCharacter(b)
      case Special(spec, _) if spec(0).isAlphaNumeric =>
        Some(spec(0))
      case Special(_, Some(char)) if char.nonEmpty && char(0).isAlphaNumeric =>
        Some(char(0))
      case _ => None
    }
  }

  def isFirstCharacterLower(str: StringElement) =
    firstCharacter(str).map(_.isBibTeXLower).getOrElse(false)

  def format(pattern: String, authors: String) = {

  }

  /* indicates whether this string starts with a lower case character at level 0 */
  def startsWithLowerCase(string: List[StringElement]) =
    string.find(el => firstCharacter(el).isDefined).isDefined

  def toList(authors: String) = {
    authors.split("(?i) and ").map { author =>

    }
  }

  def parts(name: String) = {

  }

}

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
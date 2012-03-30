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
      rep2sep(rep(block | special | "\\w+".r ^^ Word), ",") ^^ {
        case List(vonLast, jr, first) =>
          // the lastname part contains at least the last word
          var lastname = vonLast.last
          // remaining word, removing at least the last one which is in the lastname part
          val remaining = vonLast.dropRight(1)
          val (von, last) = toVonLast(remaining)
          Author(first.mkString, von, last + lastname, jr.mkString)
        case List(vonLast, first) =>
          // the lastname part contains at least the last word
          var lastname = vonLast.last
          // remaining word, removing at least the last one which is in the lastname part
          val remaining = vonLast.dropRight(1)
          val (von, last) = toVonLast(remaining)
          Author(first.mkString, von, last + lastname, "")
        case _ => EmptyAuthor
      }

    lazy val block: Parser[Block] =
      "{" ~> rep("[^\\{}]+".r ^^ Sentence | special | block) <~ "}" ^^ Block

    lazy val special: Parser[Special] =
      "\\" ~> "[^\\s{]+".r ~ opt(rep1("{") ~> "[^}]*".r <~ rep1("}")) ^^ {
        case spec ~ char => Special(spec, char)
      }

    def rep2sep[T, U](p: => Parser[T], s: => Parser[U]) =
      p ~ rep1(s ~> p) ^^ { case x ~ y => x :: y }

  }

  object NamesParser extends RegexParsers {
    override def skipWhitespace = false

    lazy val nameSep = """(?i)\s+and\s+""".r

    lazy val names =
      rep1sep(uptoNameSep, nameSep)

    lazy val uptoNameSep =
      guard(nameSep) ~> "" ^^^ Word("") |
        rep1(block | special | not(nameSep) ~> ".".r) ^^ (list => Sentence(list.mkString))

    lazy val block: Parser[Block] =
      "{" ~> rep("[^\\{}]+".r ^^ Sentence | special | block) <~ "}" ^^ Block

    lazy val special: Parser[Special] =
      "\\" ~> "[^\\s{]+".r ~ opt(rep1("{") ~> "[^}]*".r <~ rep1("}")) ^^ {
        case spec ~ char => Special(spec, char)
      }

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
        von =
          if (von.nonEmpty)
            von + " " + last + " " + part
          else
            last + " " + part
        last = ""
      } else if (isFirstCharacterLower(part)) {
        hasVon = true
        isFirst = false
        von =
          if (von.nonEmpty)
            von + " " + part
          else
            part.toString
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

  def toVonLast(parts: List[StringElement]) = {
    var von = ""
    var last = ""
    var hasVon = false
    parts.foreach { part =>
      if (isFirstCharacterLower(part) && last.nonEmpty) {
        hasVon = true
        von =
          if (von.nonEmpty)
            von + " " + last + " " + part
          else
            last + " " + part
        last = ""
      } else if (isFirstCharacterLower(part)) {
        hasVon = true
        von =
          if (von.nonEmpty)
            von + " " + part
          else
            part.toString
      } else {
        last = last + " " + part
      }
    }
    if (last.nonEmpty)
      last = last + " "
    (von, last)
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

  def format(pattern: String, author: Author) = {
    // TODO implement
    ""
  }

  def toList(authors: String) = {
    authors.split("(?i) and ").map { author =>
      NameParser.parseAll(NameParser.author, author).getOrElse {
        println("Wrong author format: " + author)
        println("This author is omitted")
        EmptyAuthor
      }
    }
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
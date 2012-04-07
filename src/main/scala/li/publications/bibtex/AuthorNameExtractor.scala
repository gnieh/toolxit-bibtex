/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

/**
 * @author Lucas Satabin
 *
 */
object AuthorNameExtractor extends RegexParsers {

  implicit def char2testable(c: Char) = new {
    def isAlphaNumeric = c.toString.matches("[a-zA-Z0-9]")
    def isBibTeXLower =
      if (c.isDigit)
        true
      else
        c.isLower
  }

  sealed trait PseudoLetter
  final case class CharacterLetter(char: Char) extends PseudoLetter {
    override def toString = char.toString
  }
  final case class BlockLetter(parts: List[PseudoLetter]) extends PseudoLetter {
    override def toString = parts.mkString("{", "", "}")
  }
  final case class SpecialLetter(command: String, arg: Option[String], withBraces: Boolean) extends PseudoLetter {
    override def toString = {
      val argument = arg match {
        case Some(a) if withBraces => "{" + a + "}"
        case Some(a) => a
        case None => ""
      }
      "{\\" + command + argument + "}"
    }
  }
  final case class Word(letters: List[PseudoLetter]) {
    override def toString = letters.mkString
  }
  final case class Sentence(words: List[Word]) {
    override def toString = words.mkString(" ")
  }

  override def skipWhitespace = false

  lazy val author = lastFirstParts | firstLastParts

  lazy val firstLastParts =
    repsep(
      word, "\\s+".r) ^^ {
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
    rep2sep(
      repsep(word, "\\s+".r),
      ",\\s*".r) ^^ {
        case List(vonLast, jr, first) =>
          // the lastname part contains at least the last word
          var lastname = vonLast.last
          // remaining word, removing at least the last one which is in the lastname part
          val remaining = vonLast.dropRight(1)
          val (von, last) = toVonLast(remaining)
          Author(first.mkString(" "), von, last + lastname, jr.mkString(" "))
        case List(vonLast, first) =>
          // the lastname part contains at least the last word
          var lastname = vonLast.last
          // remaining word, removing at least the last one which is in the lastname part
          val remaining = vonLast.dropRight(1)
          val (von, last) = toVonLast(remaining)
          Author(first.mkString(" "), von, last + lastname, "")
        case _ => EmptyAuthor
      }

  lazy val word: Parser[Word] = rep1(pseudoLetter) ^^ Word

  lazy val pseudoLetter: Parser[PseudoLetter] = special | block | character

  lazy val character: Parser[CharacterLetter] =
    "[^\\{}\\s,]".r ^^ (s => CharacterLetter(s.charAt(0)))

  lazy val block: Parser[BlockLetter] =
    "{" ~>
      rep(pseudoLetter
        | "\\s".r ^^ (s => CharacterLetter(s.charAt(0)))) <~ "}" ^^ BlockLetter

  lazy val special: Parser[SpecialLetter] =
    "{\\" ~> "'|\"|[^\\s{]+".r ~
      opt(rep1("{") ~> ("[^}\\s]*".r ^^ (s => (true, s))) <~ rep1("}")
        | ("[^}\\s]".r ^^ (s => (false, s)))) <~ "}" ^^ {
        case spec ~ Some((braces, char)) => SpecialLetter(spec, Some(char), braces)
        case spec ~ None => SpecialLetter(spec, None, false)
      }

  def rep2sep[T, U](p: => Parser[T], s: => Parser[U]) =
    p ~ rep1(s ~> p) ^^ { case x ~ y => x :: y }

  def toFirstVonLast(parts: List[Word]) = {
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
            von.trim + " " + last.trim + " " + part.toString.trim
          else
            last.trim + " " + part.toString.trim
        last = ""
      } else if (isFirstCharacterLower(part)) {
        hasVon = true
        isFirst = false
        von =
          if (von.nonEmpty)
            von.trim + " " + part.toString.trim
          else
            part.toString.trim
      } else if (isFirst) {
        first = first.trim + " " + part.toString.trim
      } else {
        last = last.trim + " " + part.toString.trim
      }
    }
    if (last.nonEmpty)
      last = last + " "
    (first, von, last)
  }

  def toVonLast(parts: List[Word]) = {
    var von = ""
    var last = ""
    var first = true
    var hasVon = true
    parts.foreach { part =>
      if (isFirstCharacterLower(part) && hasVon && last.nonEmpty) {

        von =
          if (von.nonEmpty)
            von.trim + " " + last.trim + " " + part.toString.trim
          else
            last.trim + " " + part.toString.trim
        last = ""
      } else if (isFirstCharacterLower(part) && hasVon) {
        von =
          if (von.nonEmpty)
            von.trim + " " + part.toString.trim
          else
            part.toString.trim
      } else {
        if (first)
          hasVon = false
        last = last.trim + " " + part.toString.trim
      }
      first = false
    }
    if (last.nonEmpty)
      last = last + " "
    (von, last)
  }

  /* returns the first non brace character at level 0 if any */
  def firstCharacter(str: Word): Option[Char] = {
    @tailrec
    def findFirst(letters: List[PseudoLetter]): Option[Char] = letters match {
      case (_: BlockLetter) :: tail =>
        findFirst(tail)
      case SpecialLetter(spec, _, _) :: _ if spec.contains((c: Char) => c.isLetter) =>
        spec.find(_.isLetter)
      case SpecialLetter(_, Some(char), _) :: _ =>
        char.find(_.isAlphaNumeric)
      case CharacterLetter(c) :: _ if c.isLetter =>
        Some(c)
      case _ :: tail =>
        findFirst(tail)
      case Nil => None
    }
    findFirst(str.letters)
  }

  def isFirstCharacterLower(str: Word) =
    firstCharacter(str).map(_.isBibTeXLower).getOrElse(false)

  def format(pattern: String, author: Author) = {
    // TODO implement
    ""
  }

}
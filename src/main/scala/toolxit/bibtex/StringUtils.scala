/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package toolxit.bibtex

import scala.util.parsing.combinator.RegexParsers

/**
 * @author Lucas Satabin
 *
 */
object StringUtils {

  implicit def char2testable(c: Char) = new {
    def isBibTeXLower =
      if (c.isDigit)
        true
      else
        c.isLower
  }

  object StringParser extends RegexParsers {

    lazy val string: Parser[List[StringElement]] =
      rep(block | special | "[^\\{}]+".r ^^ Word)

    lazy val block: Parser[Block] =
      "{" ~> rep("[^\\{}]+".r ^^ Sentence | special | block) <~ "}" ^^ Block

    lazy val special: Parser[Special] =
      "\\" ~> "[^\\s{]+".r ~ opt(rep1("{") ~> "[^}]*".r <~ rep1("}")) ^^ {
        case spec ~ char => Special(spec, char)
      }

  }
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
/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import scala.util.parsing.combinator.RegexParsers

/**
 * Utilities to extract the name of the different authors defined in a string.
 * @author Lucas Satabin
 *
 */
object AuthorNamesExtractor extends RegexParsers {

  override def skipWhitespace = false

  lazy val nameSep = """(?i)\s+and\s+""".r

  lazy val names =
    rep1sep(uptoNameSep, nameSep) ^^ (_.map(_.toString))

  lazy val uptoNameSep =
    guard(nameSep) ~> "" ^^^ Word("") |
      rep1(block | special | not(nameSep) ~> ".".r) ^^ (list => Sentence(list.mkString))

  lazy val block: Parser[Block] =
    "{" ~> rep("[^\\{}]+".r ^^ Sentence | special | block) <~ "}" ^^ Block

  lazy val special: Parser[Special] =
    "\\" ~> "[^\\s{]+".r ~ opt(rep("{") ~> "[^}]*".r <~ rep("}")) ^^ {
      case spec ~ char => Special(spec, char)
    }

  def toList(authors: String) = {
    parseAll(names, authors).getOrElse(Nil).map { author =>
      AuthorNameExtractor.parseAll(AuthorNameExtractor.author, author).getOrElse {
        println("Wrong author format: " + author)
        println("This author is omitted")
        EmptyAuthor
      }
    }
  }

}
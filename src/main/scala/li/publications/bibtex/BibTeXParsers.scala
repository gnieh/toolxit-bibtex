/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import tree._
import scala.util.parsing.combinator.RegexParsers

/**
 *
 * A collection of parsers to parse bibtex files.
 *
 * @author Lucas Satabin
 *
 */
object BibTeXParsers extends RegexParsers {

  lazy val bibFile: Parser[RawBibTeXDatabase] = {
    rep(string | preamble | comment ^^^ null | entry)
  } ^^ (entries => RawBibTeXDatabase(entries.filter(_ != null)))

  lazy val string: Parser[StringEntry] =
    "@" ~> ci("string") ~> "{" ~> (name <~ "=") ~ quoted <~ "}" ^^ {
      case name ~ value => StringEntry(name, value)
    }

  lazy val preamble: Parser[PreambleEntry] =
    "@" ~> ci("preamble") ~> "{" ~> concat <~ "}" ^^ PreambleEntry

  lazy val comment: Parser[Unit] =
    "@" ~> ci("comment") ~> "{" ~> concat <~ "}" ^^^ ()

  lazy val entry: Parser[BibEntry] =
    ("@" ~> name <~ "{") ~ (name <~ ",") ~ repsep(field, ",") <~ opt(",") <~ "}" ^^ {
      case name ~ key ~ fields => BibEntry(name.toLowerCase, key, fields)
    }

  lazy val field: Parser[Field] =
    (name <~ "=") ~ value ^^ { case n ~ v => RawField(n, v) }

  lazy val name: Parser[String] =
    "[^=\\s,{']+".r ^^ (_.toLowerCase)

  lazy val number: Parser[Int] = {
    "[0-9]+".r |
      "{" ~> "[0-9]+".r <~ "}" |
      "\"" ~> "[0-9]+".r <~ "\""
  } ^^ (_.toInt)

  lazy val value: Parser[Value] = braced | concat | "[^\\s,]+".r ^^ StringValue

  lazy val someText = rep1("""[^\\}{"]""".r | escaped) ^^ (_.mkString(""))

  lazy val block: Parser[Value] = quoted | braced

  lazy val concatanable: Parser[Value] = quoted | name ^^ NameValue

  lazy val concat: Parser[Value] =
    concatanable ~ opt("#" ~> repsep(concatanable, "#")) ^^ {
      case first ~ Some(next) => ConcatValue(first :: next)
      case first ~ None => first
    }

  lazy val quoted: Parser[StringValue] =
    enterBlock("\"") ~> rep(braced | someText) <~ leaveBlock("\"") ^^ { list =>
      val (start, end) =
        if (depth == 0)
          ("", "")
        else
          ("\"", "\"")
      StringValue(list.mkString(start, "", end))
    }

  lazy val braced: Parser[StringValue] =
    {
      enterBlock("{") ~> rep(braced | quoted | someText ^^ StringValue) <~ leaveBlock("}")
    } ^^ { list =>
      val (start, end) =
        if (depth == 0)
          ("", "")
        else
          ("{", "}")
      StringValue(list.map(_.value).mkString(start, "", end))
    }

  lazy val escaped: Parser[String] = "\\" ~> "[\"{}]".r

  // Helper methods
  private def ci(p: String): Parser[String] = ("(?i)" + p).r

  private var depth = 0

  // do not skip whitespaces in a block
  override def skipWhitespace = depth == 0

  def andAction[T](after: => Unit)(p: => Parser[T]) =
    p ^^ (res => { after; res })

  def resetDepth[T](p: => Parser[T]) =
    andAction(depth = 0)(p)

  def enterBlock[T](p: => Parser[T]) =
    andAction(depth += 1)(p)

  def leaveBlock[T](p: => Parser[T]) =
    andAction(depth -= 1)(p)

}
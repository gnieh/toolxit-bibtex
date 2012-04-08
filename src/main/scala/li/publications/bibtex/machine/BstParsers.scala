/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package machine

import tree._
import scala.util.parsing.combinator.RegexParsers

// TODO position parsed element to improve error messages

/**
 *
 * A bunch of parsers to parse a .bst file
 *
 * @author Lucas Satabin
 *
 */
object BstParsers extends RegexParsers {

  // extends the whiteSpace value to handle comments
  protected override val whiteSpace = "(\\s|%.*)+".r

  // ==== a .bst file ====

  lazy val bstfile: Parser[BstFile] =
    rep(entry
      | execute
      | function
      | integers
      | iterate
      | macro
      | read
      | reverse
      | sort
      | strings) ^^ BstFile

  // ==== the commands ====

  lazy val entry: Parser[BstEntry] = "ENTRY" ~>
    ("{" ~> rep(name) <~ "}") ~
    ("{" ~> rep(name) <~ "}") ~
    ("{" ~> rep(name) <~ "}") ^^ {
      case fields ~ integers ~ strings => BstEntry(fields, integers, strings)
    }

  lazy val execute: Parser[BstExecute] = "EXECUTE" ~>
    "{" ~> refName <~ "}" ^^ BstExecute

  lazy val function: Parser[BstFunction] = "FUNCTION" ~>
    ("{" ~> name <~ "}") ~
    block ^^ {
      case name ~ instr => BstFunction(name, instr)
    }

  lazy val integers: Parser[BstIntegers] = "INTEGERS" ~>
    "{" ~> rep(name) <~ "}" ^^ BstIntegers

  lazy val iterate: Parser[BstIterate] = "ITERATE" ~>
    "{" ~> refName <~ "}" ^^ BstIterate

  lazy val macro: Parser[BstMacro] = "MACRO" ~>
    ("{" ~> name <~ "}") ~
    ("{" ~> string <~ "}") ^^ { case name ~ value => BstMacro(name, value) }

  lazy val read: Parser[BstRead.type] = "READ" ^^^ BstRead

  lazy val reverse: Parser[BstReverse] = "REVERSE" ~>
    "{" ~> refName <~ "}" ^^ BstReverse

  lazy val sort: Parser[BstSort.type] = "SORT" ^^^ BstSort

  lazy val strings: Parser[BstStrings] = "STRINGS" ~>
    "{" ~> rep(name) <~ "}" ^^ BstStrings

  // ==== built-in functions and instructions ====

  lazy val instruction: Parser[BstInstruction] = (
    block
    | builtin
    | """'[^\\$&#%_{}\^~\s"0-9][^\\$&#%_{}\^~\s"]*\$?""".r ^^ (s =>
      BstPushName(s.tail.toLowerCase))
    | refName ^^ BstRefName
    | string ^^ BstPushString
    | "#-?[0-9]+".r ^^ (i => BstPushInt(i.tail.toInt)))

  lazy val block: Parser[BstBlock] =
    "{" ~> rep(instruction) <~ "}" ^^ BstBlock

  lazy val builtin: Parser[BstBuiltIn] = (
    "<" ^^^ BstInferior
    | ">" ^^^ BstSuperior
    | "=" ^^^ BstEquals
    | "+" ^^^ BstPlus
    | "-" ^^^ BstMinus
    | "*" ^^^ BstMultiply
    | ":=" ^^^ BstAssign
    | "add.period$" ^^^ BstAddPeriod
    | "call.type$" ^^^ BstCallType
    | "change.case$" ^^^ BstChangeCase
    | "chr.to.int$" ^^^ BstChrToInt
    | "cite$" ^^^ BstCite
    | "duplicate$" ^^^ BstDuplicate
    | "empty$" ^^^ BstEmpty
    | "format.name$" ^^^ BstFormatName
    | "if$" ^^^ BstIf
    | "int.to.chr$" ^^^ BstIntToChr
    | "int.to.str$" ^^^ BstIntToStr
    | "missing$" ^^^ BstMissing
    | "newline$" ^^^ BstNewline
    | "num.names$" ^^^ BstNumNames
    | "pop$" ^^^ BstPop
    | "preamble$" ^^^ BstPreamble
    | "purify$" ^^^ BstPurify
    | "quote$" ^^^ BstQuote
    | "skip$" ^^^ BstSkip
    | "stack$" ^^^ BstStack
    | "substring$" ^^^ BstSubstring
    | "swap$" ^^^ BstSwap
    | "text.length$" ^^^ BstTextLength
    | "text.prefix$" ^^^ BstTextPrefix
    | "top$" ^^^ BstTop
    | "type$" ^^^ BstType
    | "warning$" ^^^ BstWarning
    | "while$" ^^^ BstWhile
    | "width$" ^^^ BstWidth
    | "write$" ^^^ BstWrite)

  // ==== helpers ====

  // any printing character except the ten special LaTeX characters
  // ^$&#%_{}~\
  lazy val name: Parser[String] =
    """[^\\$&#%_{}\^~\s"0-9][^\\$&#%_{}\^~\s"]*""".r ^^ (_.toLowerCase)

  // this is pretty much the same as declared names, but is it possible to
  // reference an automatically declared name which ends with a `$'
  lazy val refName: Parser[String] =
    """[^\\$&#%_{}\^~\s"0-9][^\\$&#%_{}\^~\s"]*\$?""".r ^^ (_.toLowerCase)

  // a double quoted string
  lazy val string: Parser[String] = "\"" ~> "[^\"]*".r <~ "\""

}
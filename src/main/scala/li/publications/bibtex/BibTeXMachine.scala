/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import tree._
import scala.collection.mutable.{ Map, Stack }
import java.io.{ Reader, Writer }

/**
 * The BibTeXMachine is a stack machine that accepts a .bst file and
 * a BibTeX file as input and produces the corresponding output.
 *
 * @author Lucas Satabin
 *
 */
class BibTeXMachine(cites: List[String], bstReader: Reader, bibtexReader: Reader, output: Writer) {

  // ==== the stack ====
  private val stack = Stack.empty[StackValue]

  // ==== the environment ====
  // var name -> value
  private val globalVariables = Map.empty[String, Variable]
  // var name -> entry name -> value
  private val entryVariables = Map.empty[String, Map[String, Variable]]
  // field name -> entry name -> value
  private val fields = Map.empty[String, Map[String, Variable]]

  // ==== the standard entry types ====
  private val standardTypes = Set(
    "article",
    "book",
    "booklet",
    "conference",
    "inbook",
    "incollection",
    "inproceedings",
    "manual",
    "mastersthesis",
    "misc",
    "phdthesis",
    "proceedings",
    "techreport",
    "unpublished",
    // this one is added to handle unknown entry types
    "default.type")

  lazy val bstfile = try {
    BstParsers.parse(BstParsers.bstfile, bstReader) match {
      case BstParsers.Success(parsed, _) => Some(parsed)
      case error =>
        println(error)
        None
    }
  } finally {
    bstReader.close
  }

  /**
   * Runs the machine and produces the output for the input files.
   */
  def run {

    // first make sure that the stack is empty
    stack.clear

    // read the .bst file
    bstfile match {
      case Some(BstFile(commands)) if check(commands) =>
        // execute the commands in the file
        execute(commands)
      case _ => // do nothing
    }

  }

  /* this method checks different rules:
   *  - there is exactly one ENTRY command
   *  - there is exactly one READ command
   *  - ENTRY, MACRO and standard and default FUNCTION commands 
   *    are defined before the READ command
   *  - the READ commands must precede the EXECUTE, ITERATE, 
   *    REVERSE and SORT commands
   */
  private def check(commands: List[BstCommand]) = {

    var entrySeen = false
    var readSeen = false
    var standards = Set[String]()
    var errors = List[String]()

    commands.foreach {
      case BstEntry(_, _, _) =>
        // only one ENTRY command
        if (entrySeen)
          errors ::= "Duplicated ENTRY command"
        else
          entrySeen = true
        // ENTRY must precede READ
        if (readSeen)
          errors ::= "ENTRY command must precede READ command"
      case BstMacro(_, _) if readSeen =>
        // MACRO must precede READ
        errors ::= "MACRO commands must precede READ command"
      case BstFunction(name, _) if standardTypes.contains(name) =>
        standards += name
      case BstRead =>
        // only one READ command
        if (readSeen)
          errors ::= "Duplicated READ command"
        else
          readSeen = true
        // READ comes after standard types
        val missing = standardTypes &~ standards
        if (missing.nonEmpty)
          errors ::=
            "Standard types must be defined before the READ command. Missing standards types: " + missing.mkString(", ")
      case BstExecute(_) if !readSeen =>
        errors ::= "The READ command must precede EXECUTE command"
      case BstIterate(_) if !readSeen =>
        errors ::= "The READ command must precede ITERATE command"
      case BstReverse(_) if !readSeen =>
        errors ::= "The READ command must precede REVERSE command"
      case BstSort if !readSeen =>
        errors ::= "The READ command must precede SORT command"
      case _ => // all right, just go ahead
    }

    // print found errors
    errors.foreach(println _)

    errors.isEmpty

  }

  /* executes the commands defined in the bst input */
  private def execute(commands: List[BstCommand]) = commands.foreach {
    case BstEntry(fields, integers, strings) =>

  }

}

// ==== values that are pushed on the stack ====
sealed trait StackValue
final case class SStringValue(value: String) extends StackValue
final case class SNameValue(value: String) extends StackValue
final case class SIntValue(value: Int) extends StackValue
case object MissingValue extends StackValue

// ==== global variables ====
sealed trait Variable
final case class GlobalInt(value: Int) extends Variable
final case class GlobalString(value: String) extends Variable
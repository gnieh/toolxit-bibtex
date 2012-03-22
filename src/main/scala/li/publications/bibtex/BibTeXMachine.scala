/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

import tree._
import scala.util.DynamicVariable
import scala.collection.mutable.{ Map, Stack }
import java.io.{ Reader, Writer }

case class BibTeXException(msg: String, errors: List[String]) extends Exception(msg)

/**
 * The BibTeXMachine is a stack machine that accepts a .bst file and
 * a BibTeX file as input and produces the corresponding output.
 *
 * @author Lucas Satabin
 *
 */
class BibTeXMachine(cites: List[String],
                    bstReader: Reader,
                    bibtexReader: Reader,
                    output: Writer) {

  // ==== internal constants ====
  // you may change this values to accept larger strings

  // maximum length of global strings
  private val globalMax = IntVariable(250)
  // maximum length of entry strings
  private val entryMax = IntVariable(250)

  // ==== the stack ====
  private val stack = Stack.empty[StackValue]

  // ==== the environment ====
  // var name -> value
  private val globalVariables = Map.empty[String, Variable]
  // var name -> entry name -> value
  private val entryVariables = Map.empty[String, Map[String, Variable]]
  // field name -> entry name -> value
  private val fields = Map.empty[String, Map[String, Variable]]
  // name -> instructions
  private val functions = Map.empty[String, List[BstInstruction]]
  // name -> value
  private val macros = Map.empty[String, String]

  // the entries in the .bib file, sorted first following citation order
  // the SORT command may change the order of bib entries in this list
  // this list is filled when the .bib file is read by the READ command
  private var entries = List[BibEntry]()

  // contains the currently processed entry
  private val currentEntry = new DynamicVariable[Option[BibEntry]](None)

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

    // first make sure that the stack and environment is empty
    stack.clear
    cleanEnv

    // load the automatically-declared variables
    initEnv

    // read the .bst file
    bstfile match {
      case Some(BstFile(commands)) =>
        // perform checks on the commands
        check(commands)
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
  private def check(commands: List[BstCommand]) {

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

    if (errors.nonEmpty)
      throw BibTeXException("Wrong .bst file format", errors)

  }

  /* executes the commands defined in the bst input */
  private def execute(commands: List[BstCommand]): Unit = commands.foreach {
    case BstEntry(fields, integers, strings) =>
      fields.foreach { field =>
        this.fields(field) = Map()
      }
      integers.foreach { int =>
        entryVariables(int) = Map()
      }
      fields.foreach { string =>
        entryVariables(string) = Map()
      }
    case BstExecute(fun) =>
      functions.get(fun) match {
        case Some(instr) => execute(instr)
        case None =>
          throw BibTeXException("Unable to run .bst file",
            List("FUNCTION " + fun + " is not declared before it is called"))
      }
    case BstFunction(name, instr) =>
      functions(name) = instr
    case BstIntegers(names) =>
      names.foreach { name =>
        globalVariables(name) = IntVariable()
      }
    case BstIterate(fun) =>
      functions.get(fun) match {
        case Some(instr) =>
          // execute the function for each entry in the entry list
          entries.foreach { entry =>
            currentEntry.withValue(Some(entry)) {
              execute(instr)
            }
          }
        case None =>
          throw BibTeXException("Unable to run .bst file",
            List("FUNCTION " + fun + " is not declared before it is called"))
      }
    case BstMacro(name, value) =>
      macros(name) = value
    case BstRead =>
      // loads and reads the .bib database
      read
    case BstReverse(fun) =>
      functions.get(fun) match {
        case Some(instr) =>
          // execute the function for each entry in the entry list in reverse order
          entries.reverse.foreach { entry =>
            currentEntry.withValue(Some(entry)) {
              execute(instr)
            }
          }
        case None =>
          throw BibTeXException("Unable to run .bst file",
            List("FUNCTION " + fun + " is not declared before it is called"))
      }
    case BstSort =>
      // sort the entries
      sort
    case BstStrings(names) =>
      names.foreach { name =>
        globalVariables(name) = StringVariable()
      }
  }

  /* executes the instructions of a function */
  private def execute(instructions: List[BstInstruction]): Unit =
    instructions.foreach {
      case BstSuperior =>
        (popInt, popInt) match {
          case (Some(first), Some(second)) =>
            if (second > first)
              push(1)
            else
              push(0)
          case _ =>
            // error, push 0
            push(0)
        }
      case BstInferior =>
        (popInt, popInt) match {
          case (Some(first), Some(second)) =>
            if (second < first)
              push(1)
            else
              push(0)
          case _ =>
            // error, push 0
            push(0)
        }
      case BstEquals =>
        (pop, pop) match {
          case (Some(first), Some(second)) =>
            if (second == first)
              push(1)
            else
              push(0)
          case _ =>
            // error, push 0
            push(0)
        }
      case BstPlus =>
        (popInt, popInt) match {
          case (Some(first), Some(second)) =>
            push(first + second)
          case _ =>
            // error, push 0
            push(0)
        }
      case BstMinus =>
        (popInt, popInt) match {
          case (Some(first), Some(second)) =>
            push(second - first)
          case _ =>
            // error, push 0
            push(0)
        }
      case BstMultiply =>
        (popString, popString) match {
          case (Some(first), Some(second)) =>
            push(second + first)
          case _ =>
            // error, push null string
            push(NullString)
        }
    }

  /* reads and loads the .bib database */
  private def read {
    // TODO
  }

  /* sorts the entry list according to the sort.key$ field of the entries */
  private def sort {
    // TODO
  }

  // ==== helper functions ====

  /* pushes an int on the stack */
  private def push(i: Int) {
    stack.push(SIntValue(i))
  }

  /* pushes a string on the stack */
  private def push(s: String) {
    stack.push(SStringValue(s))
  }

  /* pushes a symbol on the stack */
  private def push(s: Symbol) {
    stack.push(SNameValue(s))
  }

  /* pushes a symbol on the stack */
  private def push(v: StackValue) {
    stack.push(v)
  }

  /* pops an integer from the stack. If the value is not an 
   * integer or does not exist, None is returned */
  private def popInt = {
    if (stack.isEmpty)
      None
    else
      stack.pop match {
        case SIntValue(i) => Some(i)
        case _ => None
      }
  }

  /* pops a string from the stack. If the value is not a 
   * string or does not exist, None is returned */
  private def popString = {
    if (stack.isEmpty)
      None
    else
      stack.pop match {
        case SStringValue(s) => Some(s)
        case _ => None
      }
  }

  /* pops a value from the stack. If the stack is empty returns None */
  private def pop = {
    if (stack.isEmpty)
      None
    else
      Some(stack.pop)
  }

  /* clears all entries in the environment */
  private def cleanEnv {
    globalVariables.clear
    entryVariables.clear
    fields.clear
    functions.clear
    macros.clear
    entries = Nil
    currentEntry.value = None
  }

  /* initializes the environment with built-in variables and fields */
  private def initEnv {
    fields("crossref") = Map()
    entryVariables("sort.key$") = Map()
    globalVariables("entry.may$") = entryMax
    globalVariables("global.max$") = globalMax
  }

}

// ==== values that are pushed on the stack ====
sealed trait StackValue
final case class SStringValue(value: String) extends StackValue
case object NullString extends StackValue
final case class SNameValue(value: Symbol) extends StackValue
final case class SIntValue(value: Int) extends StackValue
case object MissingValue extends StackValue

// ==== global variables ====
sealed trait Variable
final case class IntVariable(value: Option[Int] = None) extends Variable
object IntVariable {
  def apply(i: Int): IntVariable = IntVariable(Some(i))
}
final case class StringVariable(value: Option[String] = None) extends Variable
object StringVariable {
  def apply(s: String): StringVariable = StringVariable(Some(s))
}
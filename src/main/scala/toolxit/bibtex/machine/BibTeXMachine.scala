/*
* This file is part of the ToolXiT project.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package toolxit.bibtex
package machine

import tree._
import scala.util.DynamicVariable
import scala.collection.mutable.{ Map, Stack, ListBuffer, StringBuilder }
import java.io.{ Reader, Writer }

case class BibTeXException(msg: String, errors: List[String]) extends Exception(msg)

/**
 * The BibTeXMachine is a stack machine that accepts a .bst file and
 * a BibTeX file as input and produces the corresponding output.
 *
 * @author Lucas Satabin
 *
 */
class BibTeXMachine(auxReader: Reader,
                    bstReader: Reader,
                    bibReader: Reader,
                    output: Writer) {

  // ==== internal constants ====
  // you may change this values to accept larger strings

  // maximum length of global strings
  private val globalMax = IntVariable(1000)
  // maximum length of entry strings
  private val entryMax = IntVariable(1000)

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
  private val functions = Map.empty[String, FunctionVariable]
  // name -> value
  private val macros = Map.empty[String, MacroVariable]
  // value
  private val preambles = ListBuffer.empty[String]

  // the output buffer
  private val buffer = new StringBuilder

  /* searches the name in the environment.
   *  - first looks for the name in fields for current entry (if any)
   *  - if not found, looks for the name in entryVariables (if any)
   *  - if not found, looks for the name in globalVariables
   *  - if not found, looks for the name in macros
   *  - if not found, looks for the name in functions */
  private def lookup(name: String) = {
    var result =
      if (currentEntry.value.isDefined) {
        val entryName = currentEntry.value.get.key
        if (fields.contains(name)) {
          // it is a field
          fields(name).get(entryName)
        } else if (entryVariables.contains(name)) {
          // it is not a field, maybe an entry variable
          entryVariables(name).get(entryName)
        } else {
          None
        }
      } else {
        None
      }

    if (result.isEmpty) {
      // not an entry local variable
      if (globalVariables.contains(name))
        // is it a global variable?
        globalVariables.get(name)
      else if (macros.contains(name))
        // is it a macro?
        macros.get(name)
      else
        // at last lookup for a function
        functions.get(name)
    } else {
      result
    }
  }

  /* saves the variable at the appropriate place in the environment
   * if the name is not defined, throws an exception
   * only global and entry variables may be assigned */
  private def assign(name: String, value: Variable) {
    if (currentEntry.value.isDefined && entryVariables.contains(name)) {
      val entryName = currentEntry.value.get.key
      // truncate to the entry max
      val real = value match {
        case StringVariable(Some(s)) if s.length > entryMax.value.get =>
          StringVariable(s.substring(0, entryMax.value.get))
        case _ => value
      }
      entryVariables(name)(entryName) = real
    } else if (globalVariables.contains(name)) {
      // truncate to the global max
      val real = value match {
        case StringVariable(Some(s)) if s.length > globalMax.value.get =>
          StringVariable(s.substring(0, globalMax.value.get))
        case _ => value
      }
      globalVariables(name) = real
    } else {
      throw BibTeXException("Unable to run .bst file",
        List(name + " is not declared, thus cannot be assigned"))
    }
  }

  /* only global and entry variables may be assigned */
  private def canAssign(name: String) =
    (currentEntry.value.isDefined && entryVariables.contains(name)) ||
      globalVariables.contains(name)

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
    BstParsers.parseAll(BstParsers.bstfile, bstReader) match {
      case BstParsers.Success(parsed, _) => Some(parsed)
      case error =>
        println(error)
        None
    }
  } catch {
    case e: Exception =>
      e.printStackTrace
      None
  } finally {
    bstReader.close
  }

  lazy val auxfile = try {
    val aux = AuxReader.read(auxReader)
    // the BibTeX style used
    val style = aux.find(_.matches("""\\bibstyle\{[^}]+\}"""))
    // list of citations in order they appear in the LaTeX document
    // if a citation appears twice, only the first occurrence is kept
    val citationRegex = """\\citation\{([^}]+)\}"""
    val citations = aux.filter(_.matches(citationRegex)).distinct
      .map(cite =>
        citationRegex.r.findFirstMatchIn(cite).get.group(1))
    Some(AuxFile(style, citations))
  } catch {
    case e: Exception =>
      e.printStackTrace
      None
  } finally {
    auxReader.close
  }

  lazy val bibfile = try {
    BibTeXParsers.parseAll(BibTeXParsers.bibFile, bibReader) match {
      case BibTeXParsers.Success(parsed, _) => Some(parsed)
      case error =>
        println(error)
        None
    }
  } catch {
    case e: Exception =>
      e.printStackTrace
      None
  } finally {
    bibReader.close
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
        case Some(FunctionVariable(instr)) => execute(instr)
        case None =>
          throw BibTeXException("Unable to run .bst file",
            List("FUNCTION " + fun + " is not declared before it is called"))
      }
    case BstFunction(name, instr) =>
      functions(name) = FunctionVariable(instr)
    case BstIntegers(names) =>
      names.foreach { name =>
        globalVariables(name) = IntVariable()
      }
    case BstIterate(fun) =>
      functions.get(fun) match {
        case Some(FunctionVariable(instr)) =>
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
      macros(name) = MacroVariable(value)
    case BstRead =>
      // loads and reads the .bib database
      read
    case BstReverse(fun) =>
      functions.get(fun) match {
        case Some(FunctionVariable(instr)) =>
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

  /* executes the instructions of a block */
  private def execute(block: BstBlock): Unit =
    block.instructions.foreach {
      case BstPushName(name) =>
        push(name)
      case BstRefName(name) =>
        // lookup for the name and react accordingly
        lookup(name) match {
          case Some(StringVariable(Some(s))) =>
            push(s)
          case Some(StringVariable(_)) =>
            push(MissingValue)
          case Some(IntVariable(Some(i))) =>
            push(i)
          case Some(IntVariable(_)) =>
            push(MissingValue)
          case Some(MacroVariable(m)) =>
            push(m)
          case Some(FunctionVariable(code)) =>
            // call the function
            execute(code)
          case None =>
            throw BibTeXException("Unable to execute .bst file",
              List("Unknown name " + name))
        }
      case BstPushInt(i) =>
        push(i)
      case BstPushString(s) =>
        push(s)
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
            push(NullStringValue)
        }
      case BstAssign =>
        (popString, pop) match {
          case (Some(name), Some(value)) if canAssign(name) =>
            assign(name, value.toVar)
          case (Some(name), _) =>
            throw BibTeXException("Unable to run .bst file",
              List(name + " cannot be assigned"))
          case _ =>
            throw BibTeXException("Unable to run .bst file",
              List("Wrong arguments on stack"))
        }
      case BstAddPeriod =>
        popString match {
          case Some(s) =>
            val idx = s.lastIndexWhere(_ != '}')
            if (idx >= 0 && Set('.', '?', '!').contains(s(idx))) {
              push(s)
            } else {
              push(s + ".")
            }
          case None =>
            // error, push null string
            push(NullStringValue)
        }
      case BstCallType =>
        currentEntry.value match {
          case Some(entry) =>
            functions.get(entry.name) match {
              case Some(FunctionVariable(instr)) =>
                execute(instr)
              case _ if functions.contains("default.type") =>
                execute(functions("default.type").instr)
              case _ =>
                throw BibTeXException("Unable to run .bst file",
                  List("Unknown entry type: " + entry.name))
            }
          case None =>
            throw BibTeXException("Unable to run .bst file",
              List("No current entry exists"))
        }
      case BstChangeCase =>
        (popString, popString) match {
          case (Some("t" | "T"), Some(second)) =>
            // to lower case but first letters
            push(toLowerButFirst(second))
          case (Some("l" | "L"), Some(second)) =>
            // to lower case
            push(toLower(second))
          case (Some("u" | "U"), Some(second)) =>
            // to upper case
            push(toUpper(second))
          case (Some(_), Some(second)) =>
            // incorrect pattern, push back the second string
            push(second)
          case _ =>
            // error, push null string
            push(NullStringValue)
        }
      case BstChrToInt =>
        popString match {
          case Some(s) if s.length == 1 =>
            push(s(0))
          case _ =>
            // error, push zero
            push(0)
        }
      case BstCite =>
        currentEntry.value match {
          case Some(entry) =>
            push(entry.key)
          case None =>
          // TODO error
        }
      case BstDuplicate =>
        pop match {
          case Some(v) =>
            push(v)
            push(v)
          case None => // TODO Error
        }
      case BstEmpty =>
        pop match {
          case Some(MissingValue | NullStringValue) =>
            push(1)
          case Some(SStringValue(s)) if s.matches("\\s*") =>
            push(1)
          case _ => push(0)
        }
      case BstFormatName =>
        (popString, popInt, popString) match {
          case (Some(pattern), Some(authorNb), Some(authorList)) =>
            val list = AuthorNamesExtractor.toList(authorList)
            if (list.size > authorNb) {
              push(AuthorNameExtractor.format(pattern, list(authorNb)))
            } else {
              // wrong format, push null string
              push(NullStringValue)
            }
          case _ =>
            // error, push null string value
            push(NullStringValue)
        }
      case BstIf =>
        (popFunction, popFunction, popInt) match {
          case (Some(elseFun), Some(thenFun), Some(cond)) =>
            if (cond > 0)
              execute(thenFun)
            else
              execute(elseFun)
          case _ => // do nothing
        }
      case BstIntToChr =>
        popInt match {
          case Some(char) =>
            push(char.toChar.toString)
          case _ => //error, push null string
            push(NullStringValue)
        }
      case BstIntToStr =>
        popInt match {
          case Some(char) =>
            push(char.toString)
          case _ => //error, push null string
            push(NullStringValue)
        }
      case BstMissing =>
        pop match {
          case Some(MissingValue) => push(1)
          case _ => push(0)
        }
      case BstNewline =>
        if (buffer.isEmpty) {
          output.write("\n")
        } else {
          // flush the buffer to the file
          output.write(buffer.toString)
          output.flush
          // empty the buffer
          buffer.clear
        }
      case BstNumNames =>
        popString match {
          case Some(names) =>
            push(AuthorNamesExtractor.toList(names).size)
          case _ => // error, push 0
            push(0)
        }
      case BstPop =>
        // pop literal if any
        pop
      case BstPreamble =>
        push(preambles.mkString)
      case BstPurify =>
        popString match {
          case Some(str) =>
            push(purify(str))
          case _ =>
            // error, push the null string
            push(NullStringValue)
        }
      case BstQuote =>
        push("\"")
      case BstSkip =>
      // no-op
      case BstStack =>
        println(stack.mkString("\n")) // TODO improve stack formatting
        stack.clear
      case BstSubstring =>
        (popString, popInt, popInt) match {
          case (Some(string), Some(length), Some(start)) =>
            // first character is at position one in BibTeX
            push(string.substring(start - 1, length))
          case _ =>
            // error, push null string
            push(NullStringValue)
        }
    }

  /* reads and loads the .bib database */
  private def read {
    (auxfile, bibfile) match {
      case (Some(AuxFile(_, citations)), Some(BibTeXDatabase(entries))) =>

        // read all the found entries and enrich the environment with 
        // strings and preambles, and build the map of declared entries
        // in the database
        val bibEntries = Map.empty[String, BibEntry]
        entries.foreach {
          case StringEntry(name, StringValue(value)) =>
            macros(name) = MacroVariable(value)
          case StringEntry(name, concat @ ConcatValue(values)) =>
            // resolve the values
            val resolved = resolve(values)
            concat.resolved = Some(resolved)
            // set in the environment
            macros(name) = MacroVariable(resolved)
          case PreambleEntry(ConcatValue(values)) =>
            preambles += resolve(values)
          case b: BibEntry =>
            bibEntries(b.key) = b
          case _ => // should not happen, just ignore
        }

        def buildEntryList(keys: List[String]): List[BibEntry] = keys match {
          case key :: tail =>
            bibEntries.getOrElse(key, tree.UnknownEntry) :: buildEntryList(tail)
          case _ => List()
        }
        // gets the entries from the database that are in the .aux file
        this.entries = buildEntryList(citations)
      case _ => // TODO error
    }
  }

  /* sorts the entry list according to the sort.key$ field of the entries */
  private def sort {
    // get the sort key for each entry and set it in the entry
    entries.foreach {
      entry =>
        entry.sortKey = entryVariables("sort.key$").get(entry.key) match {
          case Some(StringVariable(Some(k))) => k
          case _ => //should never happen if style is correct
            throw BibTeXException("Unable to run .bst file",
              List("All entries should have defined a sort key"))
        }
    }
    // sort the keys according to the sort key
    entries = entries.sortBy(_.sortValue)
  }

  private def toLowerButFirst(s: String) = {
    // TODO
    s
  }

  private def toLower(s: String) = {
    // TODO
    s
  }

  private def toUpper(s: String) = {
    // TODO
    s
  }

  private def purify(s: String) = {
    // TODO
    s
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

  /* pops a function from the stack. If the value is not a 
   * function or does not exist, None is returned */
  private def popFunction = {
    if (stack.isEmpty)
      None
    else
      stack.pop match {
        case FunctionValue(instr) => Some(instr)
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

  /* resolves the value list to a concatenated string */
  private def resolve(values: List[Value]) =
    values.foldLeft("") { (res, cur) =>
      cur match {
        case StringValue(value) => res + value
        case NameValue(value) if (macros.contains(value)) =>
          res + macros(value)
        case NameValue(value) => res + value
        case IntValue(value) => res + value
        case EmptyValue => res
        case _: ConcatValue => // should not happen!!!
          res
      }
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
    globalVariables("entry.max$") = entryMax
    globalVariables("global.max$") = globalMax
  }

}

// ==== values that are pushed on the stack ====
sealed trait StackValue {
  def toVar: Variable
}
final case class SStringValue(value: String) extends StackValue {
  def toVar = StringVariable(value)
}
case object NullStringValue extends StackValue {
  def toVar = StringVariable()
}
final case class SIntValue(value: Int) extends StackValue {
  def toVar = IntVariable(value)
}
final case class FunctionValue(instructions: BstBlock) extends StackValue {
  def toVar = FunctionVariable(instructions)
}
case object MissingValue extends StackValue {
  def toVar = StringVariable()
}

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
final case class MacroVariable(value: String) extends Variable
final case class FunctionVariable(instr: BstBlock) extends Variable
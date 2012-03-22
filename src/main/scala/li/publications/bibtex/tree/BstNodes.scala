/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex.tree

final case class BstFile(node: List[BstCommand])

sealed trait BstCommand

final case class BstEntry(fields: List[String],
                          integers: List[String],
                          strings: List[String]) extends BstCommand

final case class BstExecute(name: String) extends BstCommand

final case class BstFunction(name: String,
                             instructions: List[BstInstruction]) extends BstCommand

final case class BstIntegers(integers: List[String]) extends BstCommand

final case class BstIterate(name: String) extends BstCommand

final case class BstMacro(name: String, value: String) extends BstCommand

case object BstRead extends BstCommand

final case class BstReverse(name: String) extends BstCommand

case object BstSort extends BstCommand

final case class BstStrings(strings: List[String]) extends BstCommand

// the different instructions that may occur in a function
sealed trait BstInstruction
// pushes the value of the given variable on the stack
final case class BstPushValue(name: String) extends BstInstruction
// pushes the name of the given variable on the stack
final case class BstPushName(name: Symbol) extends BstInstruction
// pushes the given string on the stack
final case class BstPushString(string: String) extends BstInstruction
// pushes the given integer on the stack
final case class BstPushInt(integer: Int) extends BstInstruction

// a built-in function in BibTeX
sealed trait BstBuiltIn extends BstInstruction
case object BstInferior extends BstBuiltIn
case object BstSuperior extends BstBuiltIn
case object BstEquals extends BstBuiltIn
case object BstPlus extends BstBuiltIn
case object BstMinus extends BstBuiltIn
case object BstMultiply extends BstBuiltIn
case object BstAssign extends BstBuiltIn
case object BstAddPeriod extends BstBuiltIn
case object BstCallType extends BstBuiltIn
case object BstChangeCase extends BstBuiltIn
case object BstChrToInt extends BstBuiltIn
case object BstCite extends BstBuiltIn
case object BstDuplicate extends BstBuiltIn
case object BstEmpty extends BstBuiltIn
case object BstFormatName extends BstBuiltIn
case object BstIf extends BstBuiltIn
case object BstIntToChr extends BstBuiltIn
case object BstIntToStr extends BstBuiltIn
case object BstMissing extends BstBuiltIn
case object BstNewline extends BstBuiltIn
case object BstNumNames extends BstBuiltIn
case object BstPop extends BstBuiltIn
case object BstPreamble extends BstBuiltIn
case object BstPurify extends BstBuiltIn
case object BstQuote extends BstBuiltIn
case object BstSkip extends BstBuiltIn
case object BstStack extends BstBuiltIn
case object BstSubstring extends BstBuiltIn
case object BstSwap extends BstBuiltIn
case object BstTextLength extends BstBuiltIn
case object BstTextPrefix extends BstBuiltIn
case object BstTop extends BstBuiltIn
case object BstType extends BstBuiltIn
case object BstWarning extends BstBuiltIn
case object BstWhile extends BstBuiltIn
case object BstWidth extends BstBuiltIn
case object BstWrite extends BstBuiltIn
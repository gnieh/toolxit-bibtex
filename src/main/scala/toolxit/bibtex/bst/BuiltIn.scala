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
package bst

/**
 * This trait offers a collection of built-in bst functions translated into Scala.
 *
 * @author Lucas Satabin
 *
 */
trait BuiltIn[Rendered] {

  private val formatters = scala.collection.mutable.Map.empty[String, NameFormatter]

  // character width
  private lazy val widths = {
    val res = Array.fill[Int](0200)(0)
    res(040) = 278
    res(041) = 278
    res(042) = 500
    res(043) = 833
    res(044) = 500
    res(045) = 833
    res(046) = 778
    res(047) = 278
    res(050) = 389
    res(051) = 389
    res(052) = 500
    res(053) = 778
    res(054) = 278
    res(055) = 333
    res(056) = 278
    res(057) = 500
    res(060) = 500
    res(061) = 500
    res(062) = 500
    res(063) = 500
    res(064) = 500
    res(065) = 500
    res(066) = 500
    res(067) = 500
    res(070) = 500
    res(071) = 500
    res(072) = 278
    res(073) = 278
    res(074) = 278
    res(075) = 778
    res(076) = 472
    res(077) = 472
    res(0100) = 778
    res(0101) = 750
    res(0102) = 708
    res(0103) = 722
    res(0104) = 764
    res(0105) = 681
    res(0106) = 653
    res(0107) = 785
    res(0110) = 750
    res(0111) = 361
    res(0112) = 514
    res(0113) = 778
    res(0114) = 625
    res(0115) = 917
    res(0116) = 750
    res(0117) = 778
    res(0120) = 681
    res(0121) = 778
    res(0122) = 736
    res(0123) = 556
    res(0124) = 722
    res(0125) = 750
    res(0126) = 750
    res(0127) = 1028
    res(0130) = 750
    res(0131) = 750
    res(0132) = 611
    res(0133) = 278
    res(0134) = 500
    res(0135) = 278
    res(0136) = 500
    res(0137) = 278
    res(0140) = 278
    res(0141) = 500
    res(0142) = 556
    res(0143) = 444
    res(0144) = 556
    res(0145) = 444
    res(0146) = 306
    res(0147) = 500
    res(0150) = 556
    res(0151) = 278
    res(0152) = 306
    res(0153) = 528
    res(0154) = 278
    res(0155) = 833
    res(0156) = 556
    res(0157) = 500
    res(0160) = 556
    res(0161) = 528
    res(0162) = 392
    res(0163) = 394
    res(0164) = 389
    res(0165) = 556
    res(0166) = 528
    res(0167) = 722
    res(0170) = 528
    res(0171) = 528
    res(0172) = 444
    res(0173) = 500
    res(0174) = 1000
    res(0175) = 500
    res(0176) = 500
    res
  }

  /**
   * a function that, given an entry name, returns the rendering function
   * if any. Returns `None' if none found.
   */
  val renderingFunction: String => TOption[BibEntry => Rendered]

  /**
   * Adds a ‘.’ to it if the last non‘}’ character isn’t a ‘.’, ‘?’, or ‘!’,
   * and returns this resulting string.
   */
  def addPeriod$(string: String) = {
    val periods = List('.', '!', '?')
    val lastNonBraceIndex = string.lastIndexWhere(_ != '}')
    if (lastNonBraceIndex >= 0 && periods.contains(string(lastNonBraceIndex)))
      string
    else
      string + "."
  }

  /**
   * Executes the function whose name is the entry type of an en-
   * try. For example if an entry is of type book, this function executes the
   * book function. When given as an argument to the ITERATE command,
   * call.type$ actually produces the output for the entries. For an entry
   * with an unknown type, it executes the function default.type. Thus you
   * should define (before the READ command) one function for each standard
   * entry type as well as a default.type function.
   *
   * In this case, it calls the `render*' method
   */
  def callType$(implicit entry: Option[BibEntry]): TOption[BibEntry => Rendered] =
    entry match {
      case Some(e) =>
        renderingFunction(e.name)
      case None =>
        TError("There is no current entry, unable to execute the `call.type$' function")
    }

  /**
   * Turns a string to lower case, except for the first letter and for
   * letters that appear at brace-depth strictly positive. Remember
   * that special characters are at depth 0
   */
  def toLowerButFirst(s: String) =
    StringFormatters.toLowerButFirst(s)

  /** Turns S to lower case, except parts that are at strictly positive brace-depth */
  def toLower(s: String) =
    StringFormatters.toLower(s)

  /** Turns S to upper case, except parts that are at strictly positive brace-depth */
  def toUpper(s: String) =
    StringFormatters.toUpper(s)

  /**
   * Returns the internal key of the current entry.
   */
  def cite$(implicit entry: Option[BibEntry]): TOption[String] =
    entry match {
      case Some(e) =>
        TSome(e.key)
      case None =>
        TError("There is no current entry, unable to execute the `cite$' function")
    }

  /**
   * extract the `authorNb'-th name of string `authorList' (in which names are
   * separated by and), and formats it according to specification
   * given by string `pattern'.
   */
  def formatName$(pattern: String, authorNb: Int, authorList: String) = {
    // extract author names
    val list = AuthorNamesExtractor.toList(authorList)
    if (list.size > authorNb) {
      // get the formatter associated to the pattern
      try {
        val formatter = formatters.getOrElseUpdate(pattern, new NameFormatter(pattern))
        // returns the formatted name
        TSome(formatter(list(authorNb)))
      } catch {
        case e: Exception =>
          TError("Unable to call `format,name$' function:\n", e)
      }
    } else {
      // author does not exist
      TError(authorNb + "-th author does not exist in {" + authorList + "}")
    }
  }

  def numNames$(authorList: String) =
    AuthorNamesExtractor.authorNb(authorList)

  def purify$(s: String) = {

    def purifyWord(word: Word): String =
      word.letters.foldLeft("") { (result, current) =>
        val purified = current match {
          case CharacterLetter(c) if c.isLetterOrDigit => c
          case CharacterLetter('-') => " "
          case CharacterLetter('~') => " "
          case SpecialLetter(_, Some(arg), false) => arg
          case BlockLetter(parts) => purifyWord(SimpleWord(parts))
          case _ => ""
        }
        result + purified
      }

    import StringUtils.StringParser
    StringParser.parseAll(StringParser.string, s) match {
      case StringParser.Success(res, _) =>
        TSome(res.map(purifyWord _).mkString(" "))
      case fail =>
        TError(fail.toString)
    }
  }

  def width$(s: String) = {
    def charWidth(c: Char) =
      if (c >= 0 && c < 0200)
        widths(c)
      else 0

    def letterWidth(l: PseudoLetter): Int = l match {
      case CharacterLetter(c) => charWidth(c)
      case BlockLetter(parts) =>
        parts.map(letterWidth _).sum // does not take braces into account
      case SpecialLetter("oe", _, _) => 778
      case SpecialLetter("OE", _, _) => 1014
      case SpecialLetter("ae", _, _) => 722
      case SpecialLetter("AE", _, _) => 903
      case SpecialLetter("ss", _, _) => 500
      case SpecialLetter(command, arg, _) if command(0).isLetter =>
        charWidth(command(0)) + arg.map(_.map(charWidth _).sum).getOrElse(0)
      case SpecialLetter(_, arg, _) =>
        arg.map(_.map(charWidth _).sum).getOrElse(0)
    }

    def wordWidth(w: Word): Int =
      w.letters.map(letterWidth _).sum

    import StringUtils.StringParser
    StringParser.parseAll(StringParser.string, s) match {
      case StringParser.Success(res, _) =>
        TSome(res.foldLeft(0) { (result, current) =>
          result + wordWidth(current)
        })
      case fail =>
        TError(fail.toString)
    }

  }

}
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

import com.weiglewilczek.slf4s.Logging

/**
 * This trait offers a collection of built-in bst functions translated into Scala.
 *
 * @author Lucas Satabin
 *
 */
trait BuiltIn extends Logging {

  def addPeriod$(string: String) = {
    val periods = List('.', '!', '?')
    val lastNonBraceIndex = string.lastIndexWhere(_ != '}')
    if (lastNonBraceIndex >= 0 && periods.contains(string(lastNonBraceIndex)))
      string
    else
      string + "."
  }

  def callType$(implicit name: Option[String]) = {
    //    name match {
    //      case 
    //    }
    val `call.type$` = ""
  }

  def toLowerButFirst(s: String) =
    StringFormatters.toLowerButFirst(s)

  def toLower(s: String) =
    StringFormatters.toLower(s)

  def toUpper(s: String) =
    StringFormatters.toUpper(s)

  def purify(s: String) = {
    import StringUtils.StringParser
    StringParser.parseAll(StringParser.string, s) match {
      case StringParser.Success(res, _) =>
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
        res.map(purifyWord _).mkString(" ")
      case fail =>
        logger.warn(fail.toString)
        s
    }
  }

}
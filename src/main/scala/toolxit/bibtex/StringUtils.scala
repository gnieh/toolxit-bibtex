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
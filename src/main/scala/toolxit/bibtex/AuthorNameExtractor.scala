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

/**
 * This class allows the user to extract the different author names from a string.
 *
 *
 * @author Lucas Satabin
 *
 */
object AuthorNameExtractor {

  import StringUtils._

  private object NameParser extends StringParser {

    lazy val author = lastFirstParts | firstLastParts

    lazy val firstLastParts =
      repsep(
        word, "\\s+".r) ^^ {
          case l @ some :: tail =>
            // the lastname part contains at least the last word
            var lastname = l.last
            // remaining word, removing at least the last one which is in the lastname part
            val remaining = l.dropRight(1)
            // the von part (if any) is anything between the first lower case 
            // word and the last one in lower case
            val (firstname, von, last) = toFirstVonLast(remaining)
            Author(firstname, von, last + lastname, "")
          case _ => EmptyAuthor
        }

    lazy val lastFirstParts =
      rep2sep(
        repsep(word, "\\s+".r),
        ",\\s*".r) ^^ {
          case List(vonLast, jr, first) =>
            // the lastname part contains at least the last word
            var lastname = vonLast.last
            // remaining word, removing at least the last one which is in the lastname part
            val remaining = vonLast.dropRight(1)
            val (von, last) = toVonLast(remaining)
            Author(first.mkString(" "), von, last + lastname, jr.mkString(" "))
          case List(vonLast, first) =>
            // the lastname part contains at least the last word
            var lastname = vonLast.last
            // remaining word, removing at least the last one which is in the lastname part
            val remaining = vonLast.dropRight(1)
            val (von, last) = toVonLast(remaining)
            Author(first.mkString(" "), von, last + lastname, "")
          case _ => EmptyAuthor
        }

    def rep2sep[T, U](p: => Parser[T], s: => Parser[U]) =
      p ~ rep1(s ~> p) ^^ { case x ~ y => x :: y }

  }

  def toFirstVonLast(parts: List[Word]) = {
    var first = ""
    var von = ""
    var last = ""
    var isFirst = true
    var hasVon = false
    parts.foreach { part =>
      if (isFirstCharacterLower(part) && last.nonEmpty) {
        hasVon = true
        isFirst = false
        von =
          if (von.nonEmpty)
            von.trim + " " + last.trim + " " + part.toString.trim
          else
            last.trim + " " + part.toString.trim
        last = ""
      } else if (isFirstCharacterLower(part)) {
        hasVon = true
        isFirst = false
        von =
          if (von.nonEmpty)
            von.trim + " " + part.toString.trim
          else
            part.toString.trim
      } else if (isFirst) {
        first = first.trim + " " + part.toString.trim
      } else {
        last = last.trim + " " + part.toString.trim
      }
    }
    if (last.nonEmpty)
      last = last + " "
    (first, von, last)
  }

  def toVonLast(parts: List[Word]) = {
    var von = ""
    var last = ""
    var first = true
    var hasVon = true
    parts.foreach { part =>
      if (isFirstCharacterLower(part) && hasVon && last.nonEmpty) {

        von =
          if (von.nonEmpty)
            von.trim + " " + last.trim + " " + part.toString.trim
          else
            last.trim + " " + part.toString.trim
        last = ""
      } else if (isFirstCharacterLower(part) && hasVon) {
        von =
          if (von.nonEmpty)
            von.trim + " " + part.toString.trim
          else
            part.toString.trim
      } else {
        if (first)
          hasVon = false
        last = last.trim + " " + part.toString.trim
      }
      first = false
    }
    if (last.nonEmpty)
      last = last + " "
    (von, last)
  }

  def parse(author: String) =
    NameParser.parseAll(NameParser.author, author) match {
      case NameParser.Success(res, _) => res
      case f => throw new Exception(f.toString)
    }

}
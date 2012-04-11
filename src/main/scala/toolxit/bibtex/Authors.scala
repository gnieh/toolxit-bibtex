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

class Author(val firstname: Option[String],
             val von: Option[String],
             val lastname: String,
             val jr: Option[String]) {

  override def toString =
    "first: " + firstname +
      "\nvon: " + von +
      "\nlast: " + lastname +
      "\njr: " + jr

  override def equals(other: Any) = other match {
    case Author(f, v, l, j) =>
      firstname == f && v == von && l == lastname && j == jr
    case _ => false
  }

  override def hashCode = {
    var hash = 31 + firstname.hashCode
    hash = hash * 31 + von.hashCode
    hash = hash * 31 + lastname.hashCode
    hash = hash * 31 + jr.hashCode
    hash
  }

  def format(pattern: String) = {
    ""
  }

}

object Author {
  private def option(string: String) =
    if (string.nonEmpty) Some(string) else None

  def apply(first: String, von: String, last: String, jr: String): Author = {
    val realFirst = option(first.trim)
    val realVon = option(von.trim)
    val realJr = option(jr.trim)
    new Author(realFirst, realVon, last.trim, realJr)
  }

  def unapply(author: Author): Option[(Option[String], Option[String], String, Option[String])] = {
    Some((author.firstname, author.von, author.lastname, author.jr))
  }

}

case object EmptyAuthor extends Author(None, None, "", None)
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
package renderer

import tree._

/**
 * This renderer produces a plain text version of the BibTeX database.
 *
 * @author Lucas Satabin
 *
 */
class SimpleRenderer(db: BibTeXDatabase, defaultStrings: Map[String, String])
    extends BibTeXRenderer[String](db, defaultStrings) {

  protected[this] def render(groups: List[(String, List[BibEntry])]) = {
    groups.map {
      case (key, entries) =>
        "====== " + key + " ======\n\n" +
          entries.map {
            case BibEntry(name, key, fields) =>
              "==== " + name + " (" + key + ") ====\n" +
                fields.map {
                  case Field(fname, fvalue) =>
                    "  " + fname + ": " + fvalue
                }.mkString("\n")
          }.mkString("\n\n")
    }.mkString("\n\n")
  }

  /** Renders an article */
  def renderArticle(entry: BibEntry) = renderAny(entry)

  /** Renders a book */
  def renderBook(entry: BibEntry) = renderAny(entry)

  /** Renders a booklet */
  def renderBooklet(entry: BibEntry) = renderAny(entry)

  /** Renders a conference */
  def renderConference(entry: BibEntry) = renderAny(entry)

  /** Renders an inbook */
  def renderInBook(entry: BibEntry) = renderAny(entry)

  /** Renders an incollection */
  def renderInCollection(entry: BibEntry) = renderAny(entry)

  /** Renders an inproceedings */
  def renderInProceedings(entry: BibEntry) = renderAny(entry)

  /** Renders a manual */
  def renderManual(entry: BibEntry) = renderAny(entry)

  /** Renders a masterthesis */
  def renderMasterThesis(entry: BibEntry) = renderAny(entry)

  /** Renders a misc */
  def renderMisc(entry: BibEntry) = renderAny(entry)

  /** Renders a phdthesis */
  def renderPhdThesis(entry: BibEntry) = renderAny(entry)

  /** Renders a proceedings */
  def renderProceedings(entry: BibEntry) = renderAny(entry)

  /** Renders a techreport */
  def renderTechReport(entry: BibEntry) = renderAny(entry)

  /** Renders a unpublished */
  def renderUnpublished(entry: BibEntry) = renderAny(entry)

  /** Renders an unknown */
  def renderUnknown(entry: BibEntry) = renderAny(entry)

  protected[this] def renderAny(entry: BibEntry) = entry match {
    case BibEntry(name, key, fields) =>
      "==== " + name + " (" + key + ") ====\n" +
        fields.map {
          case Field(fname, fvalue) =>
            "  " + fname + ": " + fvalue
        }.mkString("\n")
  }

}
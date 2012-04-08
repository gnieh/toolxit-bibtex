/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
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
object SimpleRenderer extends BibTeXRenderer[String] {

  def render(db: BibTeXDatabase) =
    db.entries.map {
      case BibEntry(name, key, fields) =>
        "==== " + name + " (" + key + ") ====\n" +
          fields.map {
            case Field(fname, fvalue) =>
              "  " + fname + ": " + fvalue
          }.mkString("\n")
      case _ => // do nothing
    }.mkString("\n\n")

}
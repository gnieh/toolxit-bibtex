/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package renderer

import tree.BibTeXDatabase

/**
 * A BibTeX renderer allows the user to output a BibTeX database in
 * a special format.
 *
 * @author Lucas Satabin
 *
 */
trait BibTeXRenderer[Rendered] {

  /** Takes a BibTeX database and returns its rendered string representation */
  def render(db: BibTeXDatabase): Rendered

}
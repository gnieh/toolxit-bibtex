/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex

/**
 * @author Lucas Satabin
 *
 */
object BstDSL {

  private val env = collection.mutable.Map.empty[String, String]

  def MACRO(name: Symbol)(value: String) =
    env(name.name) = value

}
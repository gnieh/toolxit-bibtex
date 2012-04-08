/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package machine

import java.io.Reader
import collection.mutable.ListBuffer

/**
 * @author Lucas Satabin
 *
 */
final case class AuxFile(style: Option[String], citations: List[String])

object AuxReader {
  def read(reader: Reader) = {
    val buffered = new java.io.BufferedReader(reader)
    var line = buffered.readLine
    val buffer = new ListBuffer[String]
    while (line != null) {
      buffer += line
      line = buffered.readLine
    }
    buffer.toList
  }

}
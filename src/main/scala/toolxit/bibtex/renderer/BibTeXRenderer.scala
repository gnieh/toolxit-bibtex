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
import scala.collection.mutable.{ HashMap, MultiMap, LinkedHashSet }

/**
 * A BibTeX renderer allows the user to output a BibTeX database in
 * a special format.
 *
 * @author Lucas Satabin
 *
 */
abstract class BibTeXRenderer[Rendered](val db: BibTeXDatabase, val defaultStrings: Map[String, String]) {

  private[this] var _groupByField: Option[String] = None
  private[this] var _groupByType = false
  private[this] var _filter: Option[Filter] = None
  private[this] var _sortBy: Option[String] = None

  private[this] var _cached: Option[Rendered] = None

  /**
   * Takes a BibTeX database and returns its rendered string representation.
   * The result is cached for more efficiency if it is called again.
   */
  def render: Rendered = _cached match {
    case Some(cached) =>
      cached
    case _ =>
      val res = render(groups)
      _cached = Some(res)
      res
  }

  /**
   * Renders the entries filter, sorted and grouped.
   * Implementors must only implement this method
   */
  protected[this] def render(groups: List[(String, List[BibEntry])]): Rendered

  /** Renders the entry identified by the key. If entry is not found, returns None */
  def render(key: String): Option[Rendered] = db.find(key).map(render _)

  /** Renders the given single entry */
  protected[this] def render(entry: BibEntry): Rendered

  /** Clears the cached value */
  def clearCache = _cached = None

  /**
   * The renderer will group entries by the given field.
   * If the field does not exist for an entry, the renderer implementation may decide
   * under which category to group this entry.
   * This method returns this renderer object to allow the user to chain calls.
   */
  def groupByField(fieldName: String): this.type = modify {
    _groupByField = Option(fieldName)
  }

  /**
   * The renderer will group entries by entry type.
   * This method returns this renderer object to allow the user to chain calls.
   */
  def groupByType: this.type = modify {
    _groupByType = true
  }

  /**
   * The renderer will only render entries matching the given filter.
   * This method returns this renderer object to allow the user to chain calls.
   */
  def filter(filter: Filter): this.type = modify {
    _filter = Option(filter)
  }

  /**
   * The renderer will sort entries by the given field.
   * If the field does not exist for an entry, the entry comes after all other. These
   * entries are sorted by key.
   * This method returns this renderer object to allow the user to chain calls.
   */
  def sortBy(fieldName: String): this.type = modify {
    _sortBy = Option(fieldName)
  }

  // ==== helper methods ====

  /* buld the group list, filtered and sorted */
  private[this] def groups: List[(String, List[BibEntry])] = {

    val groups =
      new HashMap[String, LinkedHashSet[BibEntry]] {
        def addBinding(key: String, value: BibEntry): this.type = {
          get(key) match {
            case None =>
              val set = new LinkedHashSet[BibEntry]
              set += value
              this(key) = set
            case Some(set) =>
              set += value
          }
          this
        }
      }
    var env = defaultStrings.toMap

    val filter = _filter.getOrElse(TrueFilter)

    // enrich environment with user defined strings
    db.strings.foreach {
      case StringEntry(name, value) =>
        env += (name -> value.resolve(env))
    }

    // create the groups of entries
    (_groupByField, _groupByType) match {
      case (Some(name), false) =>
        db.entries.foreach {
          case entry: BibEntry if filter.matches_?(entry) =>
            // if the entry matches the filter, add it
            val group = entry.field(name).getOrElse(EmptyValue)
            groups.addBinding(group.resolve(env), entry)
          case _ => // do nothing
        }
      case (None, true) =>
        db.entries.foreach {
          case entry: BibEntry if filter.matches_?(entry) =>
            // if the entry matches the filter, add it
            groups.addBinding(entry.name, entry)
          case _ => // do nothing
        }
      case _ =>
        db.entries.foreach {
          case entry: BibEntry if filter.matches_?(entry) =>
            // if the entry matches the filter, add it
            groups.addBinding("Entries", entry)
          case _ => // do nothing
        }
    }

    // sort elements for each group if sort field is defined
    val result = scala.collection.mutable.Map.empty[String, List[BibEntry]]
    _sortBy match {
      case Some(field) =>
        groups.keys.foreach { key =>
          result(key) = groups(key).toList.sortBy(_.field(field).getOrElse(EmptyValue))
        }
      case None =>
        groups.keys.foreach { key =>
          result(key) = groups(key).toList
        }
    }

    result.toList.sortBy(_._1)
  }

  /* The given block modifies this renderer, thus, invalidating the cache */
  private[this] def modify(block: => Any): this.type = {
    try {
      block
    } finally {
      _cached = None
    }
    this
  }

}
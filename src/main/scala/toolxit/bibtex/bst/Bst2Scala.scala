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

import java.io.Writer

/**
 * This class allows the user to generate a scala renderer class from
 * an input .bst file. The generated code is similar to what a user would
 * write using the BibTeXRenderer API.
 * The code is generated as follows:
 *  - the generated renderer is a string renderer,
 *  - each known entry type function generates one of the needed render method,
 *  - others functions are generated as private methods in the class,
 *  - macro definitions are defined as values in a companion object,
 *  - dotted names are converted to camel case names: for example `chop.name' becomes chopName
 *  - calls to built-in functions are transformed to calls to methods in the
 *    `renderer.BuiltIn' trait, that is mixed-in with the generated class.
 *
 * @author Lucas Satabin
 *
 */
class Bst2Scala(val bst: BstFile,
                val className: String,
                val packageName: String,
                out: Writer) {

  // ======== the rendering methods to generated ========

  private[this] var renderArticle: Option[String] = None
  private[this] var renderBook: Option[String] = None
  private[this] var renderBooklet: Option[String] = None
  private[this] var renderConference: Option[String] = None
  private[this] var renderInBook: Option[String] = None
  private[this] var renderInCollection: Option[String] = None
  private[this] var renderInProceedings: Option[String] = None
  private[this] var renderManual: Option[String] = None
  private[this] var renderMasterThesis: Option[String] = None
  private[this] var renderMisc: Option[String] = None
  private[this] var renderPhdThesis: Option[String] = None
  private[this] var renderProceedings: Option[String] = None
  private[this] var renderTechReport: Option[String] = None
  private[this] var renderUnpublished: Option[String] = None
  private[this] var renderUnknown: Option[String] = None

  // ======== the group rendering method to generate ========
  private[this] val render: Option[String] = None

}
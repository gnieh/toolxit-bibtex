package toolxit.bibtex
package test

import renderer._
import java.io.{ FileInputStream, InputStreamReader, FileOutputStream, OutputStreamWriter, File }
import scala.util.Properties

object Renderer extends App {

  import BibTeXParsers._

  val input = if (args.length == 0)
    "doc/metrics.bib"
  else
    args(0)

  val encoding = if (args.length < 2)
    "ISO-8859-15"
  else
    args(1)

  parseAll(bibFile,
    new InputStreamReader(new FileInputStream(input), encoding)) match {
      case Success(res, _) =>
        val renderer = new HtmlRenderer(res, Map.empty).groupByType().sortBy("year")
        val html =
          <html>
            <head>
              <style type="text/css">{ renderer.defaultCSS }</style>
            </head>
            <body>
              { renderer.render }
            </body>
          </html>

        val file = new File(Properties.userHome, "bib.html")
        file.createNewFile
        val writer = new OutputStreamWriter(
          new FileOutputStream(file), encoding)
        writer.write(html.toString)
        writer.flush
        writer.close
      case fail => println(fail)
    }

}
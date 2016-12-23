package toolxit.bibtex

import renderer._
import java.io.{ FileInputStream, InputStreamReader, FileOutputStream, OutputStreamWriter, File }
import scala.util.Properties
import toolxit.bibtex.machine.BibTeXException

object Converter extends App {

  import BibTeXParsers._

  val input = if (args.length == 0)
    "doc/metrics.bib"
  else
    args(0)

  val encoding = if (args.length < 2)
    "UTF-8"
  else
    args(1)

  parseAll(bibFile,
    new InputStreamReader(new FileInputStream(input), encoding)) match {
      case Success(res, _) ⇒
        val renderer = new HtmlRenderer(res).groupByField("year", Descending).sortBy("year")
        val bibrenderer = new BibRenderer(res).groupByType().sortBy("year")
        val html =
          <html>
            <head>
              <title>Publications in { input }</title>
              <style type="text/css">{ renderer.defaultCSS }</style>
            </head>
            <body>
              <div class="rheader">Publications in { input }</div>
              { renderer.render }
            </body>
          </html>

        val htmlFile = new File(Properties.userHome, "bib.html")

        println(s"Writing to ${htmlFile}")
        val htmlWriter = new OutputStreamWriter(
          new FileOutputStream(htmlFile), encoding)
        htmlWriter.write(html.toString)
        htmlWriter.flush
        htmlWriter.close

        val bibfile = new File(Properties.userHome, "bib.bib")
        val bibwriter = new OutputStreamWriter(
          new FileOutputStream(bibfile), encoding)
        bibwriter.write(bibrenderer.render)
        bibwriter.flush
        bibwriter.close

      case res: NoSuccess ⇒ throw new BibTeXException(s"Could not parse ${bibFile}", List(res.msg))
    }
}

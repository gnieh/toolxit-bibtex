/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications.bibtex
package test

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AuthorsTest extends FlatSpec with ShouldMatchers {

  lazy val parseBlock = (input: String) =>
    AuthorNameExtractor.parseAll(AuthorNameExtractor.block, input)

  lazy val parseAuthor = (input: String) =>
    AuthorNameExtractor.parseAll(AuthorNameExtractor.author, input)

  "The parser" should "correctly detect blocks and reject invalid blocks" in {
    // ok
    (parseBlock("{toto}").successful) should equal(true)
    (parseBlock("{toto toto}").successful) should equal(true)
    (parseBlock("{toto {toto}}").successful) should equal(true)
    (parseBlock("{{toto} {{\\toto}}}").successful) should equal(true)
    // nok
    (parseBlock("{{toto} {{\\toto}}").successful) should equal(false)
  }

  it should "correctly parse author names, and split the different parts in First von Last format" in {

    // first von last form
    (parseAuthor("AA BB").get) should equal(Author("AA", "", "BB", ""))
    (parseAuthor("AA").get) should equal(Author("", "", "AA", ""))
    (parseAuthor("AA bb").get) should equal(Author("AA", "", "bb", ""))
    (parseAuthor("aa").get) should equal(Author("", "", "aa", ""))
    (parseAuthor("AA bb CC").get) should equal(Author("AA", "bb", "CC", ""))
    (parseAuthor("AA bb CC dd EE").get) should equal(Author("AA", "bb CC dd", "EE", ""))
    (parseAuthor("AA 1B cc dd").get) should equal(Author("AA 1B", "cc", "dd", ""))
    (parseAuthor("AA 1b cc dd").get) should equal(Author("AA", "1b cc", "dd", ""))
    (parseAuthor("AA {b}B cc dd").get) should equal(Author("AA {b}B", "cc", "dd", ""))
    (parseAuthor("AA {b}b cc dd").get) should equal(Author("AA", "{b}b cc", "dd", ""))
    (parseAuthor("AA {B}b cc dd").get) should equal(Author("AA", "{B}b cc", "dd", ""))
    (parseAuthor("AA {B}B cc dd").get) should equal(Author("AA {B}B", "cc", "dd", ""))
    (parseAuthor("AA \\BB{b} cc dd").get) should equal(Author("AA \\BB{b}", "cc", "dd", ""))
    (parseAuthor("AA \\bb{b} cc dd").get) should equal(Author("AA", " \\bb{b} cc", "dd", ""))
    (parseAuthor("AA {bb} cc DD").get) should equal(Author("AA {bb}", "cc", "DD", ""))
    (parseAuthor("AA bb {cc} DD").get) should equal(Author("AA", "bb", "{cc} DD", ""))
    (parseAuthor("AA {bb} CC").get) should equal(Author("AA {bb}", "", "CC", ""))

  }

  it should "correctly parse author names, and split the different parts in von Last, Jr, First format" in {
    // von last, jr, first form
    (parseAuthor("bb CC, AA").get) should equal(Author("AA", "bb", "CC", ""))
    (parseAuthor("bb CC, aa").get) should equal(Author("aa", "bb", "CC", ""))
    (parseAuthor("bb CC dd EE, AA").get) should equal(Author("AA", "bb CC dd", "EE", ""))
    (parseAuthor("bb, AA").get) should equal(Author("AA", "", "bb", ""))
    (parseAuthor("BB,").get) should equal(Author("", "", "BB", ""))
    (parseAuthor("bb CC,XX, AA").get) should equal(Author("AA", "bb", "CC", "XX"))
    (parseAuthor("bb CC,xx, AA").get) should equal(Author("AA", "bb", "CC", "xx"))
    (parseAuthor("BB,, AA").get) should equal(Author("AA", "", "BB", ""))

  }

  it should "correctly recognize special characters and their case in First von Last format" in {
    (parseAuthor("Paul \\'Emile Victor").get) should equal(Author("Paul \\'Emile", "", "Victor", ""))
    (parseAuthor("Paul {\\'E}mile Victor").get) should equal(Author("Paul {\\'E}mile", "", "Victor", ""))
    (parseAuthor("Paul \\'emile Victor").get) should equal(Author("Paul", "\\'emile", "Victor", ""))
    (parseAuthor("Paul {\\'e}mile Victor").get) should equal(Author("Paul", "{\\'e}mile", "Victor", ""))

  }

  it should "correctly recognize special characters and their case in von Last, Jr, First format" in {
    (parseAuthor("Victor, Paul \\'Emile").get) should equal(Author("Paul \\'Emile", "", "Victor", ""))
    (parseAuthor("Victor, Paul {\\'E}mile").get) should equal(Author("Paul {\\'E}mile", "", "Victor", ""))
    (parseAuthor("Victor, Paul \\'emile").get) should equal(Author("Paul \\'emile", "", "Victor", ""))
    (parseAuthor("Victor, Paul {\\'e}mile").get) should equal(Author("Paul {\\'e}mile", "", "Victor", ""))
  }

}
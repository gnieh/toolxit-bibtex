/* This file is part of blue.publications.li
 * © Copyright Martin Monperrus, Lucas Satabin. All rights reserved.
 */
package li.publications

/**
 * @author Lucas Satabin
 *
 */
package object bibtex {

  /**
   * This object contains the default global names, such as the three letters
   * month names. Names contained in this default map are those in the plain.bst
   * bibTeX style.
   *
   * @author Lucas Satabin
   *
   */
  lazy val DefaultGlobals = Map(
    "jan" -> "January",
    "feb" -> "February",
    "mar" -> "March",
    "apr" -> "April",
    "may" -> "May",
    "jun" -> "June",
    "jul" -> "July",
    "aug" -> "August",
    "sep" -> "September",
    "oct" -> "October",
    "nov" -> "November",
    "dec" -> "December",
    "acmcs" -> "ACM Computing Surveys",
    "acta" -> "Acta Informatica",
    "cacm" -> "Communications of the ACM",
    "ibmjrd" -> "IBM Journal of Research and Development",
    "ibmsj" -> "IBM Systems Journal",
    "ieeese" -> "IEEE Transactions on Software Engineering",
    "ieeetc" -> "IEEE Transactions on Computers",
    "ieeetcad" -> "IEEE Transactions on Computer-Aided Design of Integrated Circuits",
    "ipl" -> "Information Processing Letters",
    "jacm" -> "Journal of the ACM",
    "jcss" -> "Journal of Computer and System Sciences",
    "scp" -> "Science of Computer Programming",
    "sicomp" -> "SIAM Journal on Computing",
    "tocs" -> "ACM Transactions on Computer Systems",
    "tods" -> "ACM Transactions on Database Systems",
    "tog" -> "ACM Transactions on Graphics",
    "toms" -> "ACM Transactions on Mathematical Software",
    "toois" -> "ACM Transactions on Office Information Systems",
    "toplas" -> "ACM Transactions on Programming Languages and Systems",
    "tcs" -> "Theoretical Computer Science")
}
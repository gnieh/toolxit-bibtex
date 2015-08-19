This project is part of ToolXiT

It provides some utilities in Scala to work with BibTeX files.
For example, it provides following features:
 - Parsing of .bib files
 - Parsing of .bst files
 - Implementation of the BibTeX stack machine
 - Renderers for .bib files to produce plain text, xml, html representation of a BibTeX database
 - A .bst to Scala translator (TODO)

You can depend on this library by using this line in your sbt script:

    libraryDependencies += "org.openmole" %% "toolxit-bibtex" % "0.1"

or in a Maven pom.xml:

    <dependency>
      <groupId>org.openmole</groupId>
      <artifactId>toolxit-bibtex</artifactId>
      <version>0.1-SNAPSHOT</version>
    </dependency>


###Development###

The library can be generated locally using:
`sbt publish-local`

To release in one step, use:
`sbt 'release with-defaults'`

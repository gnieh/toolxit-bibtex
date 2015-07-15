
import SonatypeKeys._

import com.typesafe.sbt.pgp.PgpKeys._

sonatypeSettings

organization := "org.openmole"

name := "toolxit-bibtex"

publishMavenStyle := true

publishArtifact := false

publishArtifact in Test := false

scalariformSettings

releaseSettings

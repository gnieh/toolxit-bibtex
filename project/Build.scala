/**
 * Copyright (C) 2015 Jonathan Passerat-Palmbach
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._
import sbt.{CrossVersion, _}
import Keys._

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform._

trait Settings <: Build {

  lazy val javaVersion = settingKey[String]("Defines Java version according to Scala version in X compile'")

  override def settings = super.settings ++ Seq (
    scalaVersion := "2.12.1",
    crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.1"),
    javaVersion := { CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor >= 11 => "1.8"
      case Some((2, 10)) =>"1.7"
    }},
    javacOptions in (Compile, compile) ++= Seq("-source", javaVersion.value, "-target", javaVersion.value),
    scalacOptions ++= Seq(s"-target:jvm-${javaVersion.value}", "-deprecation", "-feature")
  )

  lazy val scalariform = scalariformSettings ++ Seq(
    ScalariformKeys.preferences :=
      ScalariformKeys.preferences.value
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(RewriteArrowSymbols, true) )

  lazy val commonSettings = settings ++ scalariform

  lazy val nonPublishSettings =
    commonSettings ++
      Seq (publishTo := Some(Resolver.file("Unused transient repository", file("target/unusedrepo"))))

  lazy val publishSettings =
    commonSettings ++
      Seq (
      organization := "toolxit.bibtex",
      resolvers ++= Seq("ISC-PIF" at "http://maven.iscpif.fr/public/", Resolver.sonatypeRepo("snapshots"), Resolver.sonatypeRepo("releases")),
      publishTo <<= isSnapshot { snapshot =>
        val nexus = "https://oss.sonatype.org/"
        if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
        else Some("releases" at nexus + "service/local/staging/deploy/maven2")
      },
      pomIncludeRepository := { _ => false},
      licenses := Seq("GPLv3" -> url("http://www.gnu.org/licenses/")),
      homepage := Some(url("https://github.com/jopasserat/toolxit-bibtex")),
      scmInfo := Some(ScmInfo(url("https://github.com/jopasserat/toolxit-bibtex.git"), "scm:git:git@github.com:jopasserat/toolxit-bibtex.git")),
      // To sync with Maven central, you need to supply the following information:
      pomExtra := {
        <!-- Developer contact information -->
          <developers>
            <developer>
              <id>jopasserat</id>
              <name>Jonathan Passerat-Palmbach</name>
              <url>https://github.com/jopasserat/</url>
            </developer>
          </developers>
      }
    )

}

trait ToolxitBibtexComponents <: Settings {

  lazy val macros = Project(
    "macros",
    file("macros"),
    settings = nonPublishSettings ++ Seq(
      // macros syntax is different from 2.10 to 2.11+...
      unmanagedSourceDirectories in Compile <+= (sourceDirectory in Compile, scalaVersion){
        (s,v) =>
          val scalaMajor = if (v.startsWith("2.10")) "2.10" else "2.11+"
          s / s"scala-$scalaMajor"
      },
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value
          // in Scala 2.10, quasiquotes are provided by macro paradise
          case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.1.0" cross CrossVersion.binary)
        }
      },
      publishArtifact := false
    )
  )

  lazy val core = Project(
    "core",
    file("core"),
    settings = nonPublishSettings ++ Seq(
      libraryDependencies ++= Seq (
        "org.freemarker" % "freemarker" % "2.3.19",
        "junit" % "junit" % "4.10" % "test",
        "org.scalatest" % s"scalatest" % "3.0.1" % "test" cross CrossVersion.binary
      ),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, the now external xml and parser combinators shall be added
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value ++ Seq(
              "org.scala-lang.modules" %% "scala-xml" % "1.0.6",
              "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5"
            )
          // in Scala 2.10, xml and parser combinators are shipped with the library
          case Some((2, 10)) =>
            libraryDependencies.value
        }
      },
      publishArtifact := false
    )
  ) dependsOn(macros)

}

trait ToolxitBibtex <: Settings with ToolxitBibtexComponents {

  lazy val toolxitBibtex = Project(
    "toolxit-bibtex",
    file("target/bundle")
    ).settings (
      (publishSettings ++
      osgiSettings ++
      Seq(
        organization := "org.openmole",
        importPackage := Seq("scala.*"),
        privatePackage := Seq("!scala.*", "*"),
        bundleSymbolicName := "toolxit.bibtex",
        exportPackage := Seq("toolxit.bibtex.*"),
        publishArtifact in (Compile, packageBin) := true,
        publishArtifact in (Test, packageBin) := false
      )): _* ) dependsOn(macros, core)
}

object ToolxitBibtexBuild extends Build with ToolxitBibtex

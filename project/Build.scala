import com.typesafe.sbt.osgi.OsgiKeys._
import com.typesafe.sbt.osgi.SbtOsgi._
import sbt._
import Keys._

import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform._

trait Settings <: Build {

  override def settings = super.settings ++ Seq (
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.10.5", "2.11.0", "2.11.1", "2.11.2", "2.11.3", "2.11.4",
      "2.11.5", "2.11.6", "2.11.7"),
    javacOptions in (Compile, compile) ++= Seq("-source", "1.7", "-target", "1.7"),
    scalacOptions ++= Seq("-target:jvm-1.7", "-deprecation")
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
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, quasiquotes are available in the standard distribution
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
          libraryDependencies.value
          // in Scala 2.10, quasiquotes are provided by macro paradise
          case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % "2.1.0-M5" cross CrossVersion.binary)
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
        "org.scalatest" % s"scalatest" % "2.2.4" % "test" cross CrossVersion.binary
      ),
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if Scala 2.11+ is used, the now external xml and parser combinators shall be added
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value ++ Seq(
              "org.scala-lang.modules" %% "scala-xml" % "1.0.3",
              "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
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

import java.nio.charset.StandardCharsets

import com.github.kxbmap.sbt.jooq.JooqCodegen.autoImport._
import org.h2.tools.RunScript

organization := "com.contxt"

organizationName := "Street Contxt"

organizationHomepage := Some(url("http://streetcontxt.com"))

developers := List(
  Developer("agenovese", "Angelo Genovese", "angelo@genovese.ca", url("https://github.com/agenovese/"))
)

scmInfo := Some(ScmInfo(url("https://github.com/streetcontxt/"),
  "https://github.com/StreetContxt/jooq-scala-macro.git",
  Some("git@github.com:StreetContxt/jooq-scala-macro.git")))

name := "Jooq Scala Macro"

version := sys.props
  .get("TRAVIS_BUILD_NUMBER")
  .map(v => v + ".0")
  .getOrElse("LOCAL-SNAPSHOT")

scalaVersion := "2.11.8"

homepage := Some(url("https://github.com/StreetContxt/jooq-scala-macro"))

startYear := Some(2016)

licenses := Seq(
  ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0"))
)

description :=
  """A public type provider macro which produces
    |a set of objects and classes based on those
    |generated by Jooq""".stripMargin

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Ymacro-debug-lite",
  "-Xlint"
)

lazy val paradiseDependency = "org.scalamacros" % "paradise_2.11.8" % "2.1.0"

resolvers += Resolver.mavenLocal

libraryDependencies ++= Seq(
  "org.jooq" % "jooq" % "3.10.2",
  "org.jooq" %% "jooq-scala" % "3.10.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  paradiseDependency,
  "org.specs2" %% "specs2-core" % "3.8.4" % "test",
  "com.h2database" % "h2" % "1.4.192" % "jooq",
  "com.h2database" % "h2" % "1.4.192" % "test"
)

jooqVersion := "3.10.2"

addCompilerPlugin(paradiseDependency)

enablePlugins(JooqCodegen)

lazy val populateH2 = TaskKey[Unit]("populate-h2", "populates the H2 Database")

coverageHighlighting := false

populateH2 := {
  Class.forName("org.h2.Driver")
  RunScript.execute("jdbc:h2:./target/test-db", "", "",
    "src/test/resources/test-db.sql", StandardCharsets.US_ASCII, false)
}

jooqCodegenConfigFile := Some(file("src/test/resources/jooq-codegen.xml"))

jooqCodegenTargetDirectory := baseDirectory.value / "jooq"

unmanagedSourceDirectories in Test += jooqCodegenTargetDirectory.value

sourceDirectories in Test += jooqCodegenTargetDirectory.value

compileOrder in Test := CompileOrder.JavaThenScala

jooqCodegen := {
  populateH2.value;
  jooqCodegen.value
}

(compile in Test) := {
  jooqCodegen.value;
  (compile in Test).value
}

(test in Test) := {
  populateH2.value;
  (test in Test).value
}

(testOnly in Test) := {
  populateH2.value;
  (testOnly in Test).value
}

(testQuick in Test) := {
  populateH2.value;
  (testQuick in Test).value
}

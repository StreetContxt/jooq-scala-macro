organization := "io.github.streetcontxt",
name := "jooq-scala-macro"
description := "A public type provider macro which produces a set of objects and classes based on those generated by Jooq"
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
homepage := Some(url("https://github.com/streetcontxt/jooq-scala-macro")),
developers := List(
  Developer(
    "agenovese",
    "Angelo Gerard Genovese",
    "angelo.gerard.genovese@gmail.com",
    url("https://github.com/agenovese")
  )
)

resolvers += Resolver.mavenLocal

scalaVersion := "2.13.3"
crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.3")

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  // uncomment to see generated code
  // "-Ymacro-debug-lite",
  "-Xlint"
)

lazy val scalaBefore213 = settingKey[Boolean]("Compiling using Scala < 2.13")

def configureVersionSpecificSettings() = Seq(
  scalaBefore213 := {
    val sv = scalaVersion.value
    (sv startsWith "2.11.") || (sv startsWith "2.12.")
  },
  libraryDependencies ++= {
    val jooqVersion = if (scalaBefore213.value) "3.10.8" else "3.13.2"
    Seq(
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Compile,
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided,
      "org.jooq" % "jooq" % jooqVersion,
      "org.jooq" %% "jooq-scala" % jooqVersion
    )
  },
  libraryDependencies ++= {
    if (scalaBefore213.value)
      Seq(
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
        "org.scala-lang.modules" %% "scala-collection-compat" % "2.1.6",
      )
    else
      Nil
  },
  scalacOptions ++= {
    if (scalaBefore213.value) Nil
    else Seq("-Ymacro-annotations")
  }
)

configureVersionSpecificSettings()

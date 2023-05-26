import scala.Seq
val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "demo",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies := Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-effect" % "3.2.9" withSources() withJavadoc()
    )
  )
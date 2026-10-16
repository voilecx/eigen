ThisBuild / organization := "eigen"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / licenses := Seq("GPL-3.0-only" -> url("https://www.gnu.org/licenses/gpl-3.0.html"))

// Paper 26.1+ runs only on Java 25.
ThisBuild / scalacOptions ++= Seq("-release", "25", "-deprecation", "-feature", "-Wunused:all")

val paperApiVersion = "26.2.build.134-stable"
val packetEventsVersion = "2.14.0"
val munitVersion = "1.3.6"

lazy val root = (project in file("."))
  .aggregate(core, paper)
  .settings(
    name := "eigen",
    publish / skip := true,
  )

// Feature extraction, masks, windows and model runtime. Must not depend on Bukkit/Paper,
// so the same code runs on the server and in offline replay.
lazy val core = (project in file("core"))
  .settings(
    name := "eigen-core",
    libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  )

// Paper/Folia adapter: packet listeners, scheduling, commands.
lazy val paper = (project in file("paper"))
  .dependsOn(core)
  .settings(
    name := "eigen-paper",
    resolvers ++= Seq(
      "papermc" at "https://repo.papermc.io/repository/maven-public/",
      "codemc-releases" at "https://repo.codemc.io/repository/maven-releases/",
    ),
    libraryDependencies ++= Seq(
      "io.papermc.paper" % "paper-api" % paperApiVersion % Provided,
      // Supplied at runtime by the PacketEvents plugin (see `depend` in plugin.yml).
      "com.github.retrooper" % "packetevents-spigot" % packetEventsVersion % Provided,
    ),
    Compile / resourceGenerators += Def.task {
      val out = (Compile / resourceManaged).value / "plugin.yml"
      IO.write(out, pluginYml(version.value, scalaVersion.value))
      Seq(out)
    }.taskValue,
    // The Scala runtime is downloaded by Paper's library loader instead of being shaded:
    // relocating scala.* breaks the runtime.
    assemblyPackageScala / assembleArtifact := false,
    assembly / assemblyJarName := s"Eigen-${version.value}.jar",
  )

def pluginYml(version: String, scalaVersion: String): String =
  s"""name: Eigen
     |version: '$version'
     |main: eigen.paper.EigenPlugin
     |description: Open-source ML combat anti-cheat
     |api-version: '1.13'
     |folia-supported: true
     |depend: [packetevents]
     |libraries:
     |  - org.scala-lang:scala3-library_3:$scalaVersion
     |""".stripMargin

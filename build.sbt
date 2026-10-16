ThisBuild / organization := "eigen"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / licenses := Seq("GPL-3.0-only" -> url("https://www.gnu.org/licenses/gpl-3.0.html"))

// Paper 26.1+ runs only on Java 25.
ThisBuild / scalacOptions ++= Seq("-release", "25", "-deprecation", "-feature", "-Wunused:all")

val paperApiVersion = "26.2.build.134-stable"
val packetEventsVersion = "2.14.0"
val jsoniterVersion = "2.41.5"
val macwireVersion = "2.6.7"
val munitVersion = "1.3.6"

val paperRepos = Seq(
  "papermc" at "https://repo.papermc.io/repository/maven-public/",
  "codemc-releases" at "https://repo.codemc.io/repository/maven-releases/",
)

lazy val root = (project in file("."))
  .aggregate(core, commands, paper)
  .settings(
    name := "eigen",
    publish / skip := true,
  )

// Simulation, features, windows and the wire codec. No Bukkit, Paper or PacketEvents types,
// so the same code runs on the server, in tests and in offline replay.
lazy val core = (project in file("core"))
  .settings(
    name := "eigen-core",
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % Provided,
      "org.scalameta" %% "munit" % munitVersion % Test,
    ),
  )

// A small Scala 3 DSL over Paper's Brigadier command API.
lazy val commands = (project in file("commands"))
  .settings(
    name := "eigen-commands",
    resolvers ++= paperRepos,
    libraryDependencies += "io.papermc.paper" % "paper-api" % paperApiVersion % Provided,
  )

// Paper/Folia adapter: packet listeners, transport, collection, commands.
lazy val paper = (project in file("paper"))
  .dependsOn(core, commands)
  .settings(
    name := "eigen-paper",
    resolvers ++= paperRepos,
    libraryDependencies ++= Seq(
      "io.papermc.paper" % "paper-api" % paperApiVersion % Provided,
      // Supplied at runtime by the PacketEvents plugin (see `depend` in plugin.yml).
      "com.github.retrooper" % "packetevents-spigot" % packetEventsVersion % Provided,
      // Compile-time only: autowire expands to plain constructor calls.
      "com.softwaremill.macwire" %% "macros" % macwireVersion % Provided,
      "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % jsoniterVersion % Provided,
      "org.scalameta" %% "munit" % munitVersion % Test,
    ),
    Compile / resourceGenerators += Def.task {
      val out = (Compile / resourceManaged).value / "plugin.yml"
      IO.write(out, pluginYml(version.value, scalaVersion.value))
      Seq(out)
    }.taskValue,
    // Only our own classes go into the jar. Third-party runtime libraries are listed under
    // `libraries` in plugin.yml and downloaded by Paper's library loader; relocating scala.*
    // would break the runtime.
    assembly / assemblyOption ~= (_.withIncludeScala(false).withIncludeDependency(false)),
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
     |  - com.github.plokhotnyuk.jsoniter-scala:jsoniter-scala-core_3:$jsoniterVersion
     |permissions:
     |  eigen.command:
     |    default: op
     |  eigen.alerts:
     |    default: op
     |  eigen.collect:
     |    default: op
     |  eigen.admin:
     |    default: op
     |""".stripMargin

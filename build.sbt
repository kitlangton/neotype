inThisBuild(
  List(
    organization := "io.github.kitlangton",
    scalaVersion := "3.3.5",
    homepage     := Some(url("https://github.com/kitlangton/neotype")),
    licenses     := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("kitlangton", "Kit Langton", "kit.langton@gmail.com", url("https://github.com/kitlangton"))
    ),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

Global / onChangedBuildSource := ReloadOnSourceChanges

////////////////////////
// sbt-github-actions //
////////////////////////
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

ThisBuild / githubWorkflowEnv := Map("JAVA_OPTS" -> "-Xmx4g")
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches :=
  Seq(
    RefPredicate.StartsWith(Ref.Tag("v")),
    RefPredicate.Equals(Ref.Branch("main"))
  )

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    commands = List("ci-release"),
    name = Some("Publish project"),
    env = Map(
      "PGP_PASSPHRASE"    -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET"        -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}"
    )
  )
)

/////////////////////////
// Project Definitions //
/////////////////////////

lazy val jsoniterVersion       = "2.33.2"
lazy val circeVersion          = "0.14.10"
lazy val tapirVersion          = "1.11.14"
lazy val zioVersion            = "2.1.15"
lazy val zioConfigVersion      = "4.0.3"
lazy val zioSchemaVersion      = "1.6.1"
lazy val zioJsonVersion        = "0.7.39"
lazy val chimneyVersion        = "1.7.3"
lazy val calibanVersion        = "2.9.2"
lazy val doobieVersion         = "1.0.0-RC7"
lazy val upickleVersion        = "4.1.0"
lazy val cirisVersion          = "3.7.0"
lazy val zioInteropCatsVersion = "23.1.0.3"

val sharedSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-Xcheck-macros",
    "-Wunused:all"
  ),
  libraryDependencies ++= Seq(
    "dev.zio" %%% "zio-test"     % zioVersion % Test,
    "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
  )
)

lazy val root = (project in file("."))
  .settings(
    name := "neotype"
  )
  .aggregate(
    circe.js,
    circe.jvm,
    core.js,
    core.jvm,
    jsoniter.js,
    jsoniter.jvm,
    playJson.js,
    playJson.jvm,
    examples,
    tapir.js,
    tapir.jvm,
    zioConfig,
    zioJson.js,
    zioJson.jvm,
    zioQuill,
    zioSchema.js,
    zioSchema.jvm,
    zioTest.js,
    zioTest.jvm,
    chimney.js,
    chimney.jvm,
    caliban.jvm,
    doobie.jvm,
    upickle.js,
    upickle.jvm,
    ciris.js,
    ciris.jvm
  )

lazy val core = (crossProject(JSPlatform, JVMPlatform) in file("modules/core"))
  .settings(
    name := "neotype",
    sharedSettings
  )

lazy val circe = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-circe"))
  .settings(
    name := "neotype-circe",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core"   % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val jsoniter = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-jsoniter"))
  .settings(
    name := "neotype-jsoniter",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % jsoniterVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val playJson = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-play-json"))
  .settings(
    name := "neotype-play-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.playframework" %%% "play-json" % "3.1.0-M1"
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val tapir = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-tapir"))
  .settings(
    name := "neotype-tapir",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %%% "tapir-core"         % tapirVersion,
      "com.softwaremill.sttp.tapir" %%% "tapir-json-pickler" % tapirVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioConfig = (project in file("modules/neotype-zio-config"))
  .settings(
    name := "neotype-zio-config",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-config"          % zioConfigVersion,
      "dev.zio" %% "zio-config-magnolia" % zioConfigVersion
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")

lazy val zioSchema = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-schema"))
  .settings(
    name := "neotype-zio-schema",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-schema"      % zioSchemaVersion,
      "dev.zio" %%% "zio-schema-json" % zioSchemaVersion % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioJson = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-json"))
  .settings(
    name := "neotype-zio-json",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-json" % zioJsonVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val zioQuill = (project in file("modules/neotype-zio-quill"))
  .settings(
    name := "neotype-zio-quill",
    sharedSettings,
    libraryDependencies ++= Seq(
      "io.getquill"   %% "quill-jdbc-zio" % "4.8.6",
      "org.postgresql" % "postgresql"     % "42.7.5"  % Test,
      "com.h2database" % "h2"             % "2.3.232" % Test
    )
  )
  .dependsOn(core.jvm % "compile->compile;test->test")

lazy val zioTest = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-zio-test"))
  .settings(
    name := "neotype-zio-test",
    sharedSettings,
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-test"          % zioVersion,
      "dev.zio" %%% "zio-test-magnolia" % zioVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val chimney = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-chimney"))
  .settings(
    name := "neotype-chimney",
    sharedSettings,
    libraryDependencies ++= Seq("io.scalaland" %%% "chimney" % chimneyVersion)
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val caliban = (crossProject(JVMPlatform) in file("modules/neotype-caliban"))
  .settings(
    name := "neotype-caliban",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban" % calibanVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val doobie = (crossProject(JVMPlatform) in file("modules/neotype-doobie"))
  .settings(
    name := "neotype-doobie",
    sharedSettings,
    libraryDependencies ++= Seq(
      "org.tpolecat"  %% "doobie-core"     % doobieVersion,
      "org.tpolecat"  %% "doobie-postgres" % doobieVersion % Test,
      "com.h2database" % "h2"              % "2.3.232"     % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val upickle = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-upickle"))
  .settings(
    name := "neotype-upickle",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % upickleVersion
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val ciris = (crossProject(JSPlatform, JVMPlatform) in file("modules/neotype-ciris"))
  .settings(
    name := "neotype-ciris",
    sharedSettings,
    libraryDependencies ++= Seq(
      "is.cir"  %%% "ciris"            % cirisVersion,
      "dev.zio" %%% "zio-interop-cats" % zioInteropCatsVersion % Test
    )
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
  .settings(
    name := "neotype-examples",
    sharedSettings,
    publish / skip := true
  )
  .dependsOn(core.jvm, zioJson.jvm, zioQuill)

addCommandAlias("prepare", "scalafixAll;scalafmtAll;githubWorkflowGenerate")

welcomeMessage

def welcomeMessage =
  onLoadMessage := {
    import scala.Console

    def header(text: String): String = s"${Console.RED}$text${Console.RESET}"
    def item(text: String): String   = s"${Console.GREEN}> ${Console.CYAN}$text${Console.RESET}"

    s"""|${header(" _   _ _____ _____ _______   _______ _____ ")}
        |${header("| \\ | |  ___|  _  |_   _\\ \\ / | ___ |  ___|")}
        |${header("|  \\| | |__ | | | | | |  \\ V /| |_/ | |__ ")}
        |${header("| . ` |  __|| | | | | |   \\ / |  __/|  __|")}
        |${header("| |\\  | |___\\ \\_/ / | |   | | | |   | |___")}
        |${header("\\_| \\_\\____/ \\___/  \\_/   \\_/ \\_|   \\____/")}
        |${header("——————————————————————————————————————————")}
        |${header("———————— NEWTYPES + REFINED TYPES ————————")}
        |${header("——————————————————————————————————————————")}

        |Useful sbt tasks:

        |${item("prepare")} - Runs scalafix and scalafmt on all files
        |${item("~compile")} - Compiles all modules (file-watch enabled)
        |${item("test")} - Runs all tests

    """.stripMargin

  }

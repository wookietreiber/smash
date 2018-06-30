// ----------------------------------------------------------------------------
// sbt plugins
// ----------------------------------------------------------------------------

enablePlugins(BuildInfoPlugin)
enablePlugins(GitVersioning)
enablePlugins(ScalaNativePlugin)

// ----------------------------------------------------------------------------
// basic project settings
// ----------------------------------------------------------------------------

name := "smash"

git.baseVersion in ThisBuild := "0.0.1"

scalaVersion in ThisBuild := "2.11.12"

libraryDependencies += "com.github.scopt" %%% "scopt" % "3.7.0"
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"
libraryDependencies += "com.lihaoyi" %%% "utest" % "0.6.4" % "test"

testFrameworks := Seq(new TestFramework("utest.runner.Framework"))

nativeLinkStubs := true

// ----------------------------------------------------------------------------
// scala compiler options
// ----------------------------------------------------------------------------

scalacOptions in ThisBuild ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked"
)

// ----------------------------------------------------------------------------
// scaladoc options
// ----------------------------------------------------------------------------

scalacOptions in (Compile, doc) ++= Seq(
  "-diagrams",
  "-groups",
  "-implicits"
)

// ----------------------------------------------------------------------------
// build info
// ----------------------------------------------------------------------------

buildInfoKeys := Seq[BuildInfoKey](name, version)

buildInfoPackage := "smash"

// ----------------------------------------------------------------------------
// formatting
// ----------------------------------------------------------------------------

scalafmtVersion := "1.4.0"

scalafmtOnCompile := true

// ----------------------------------------------------------------------------
// linting
// ----------------------------------------------------------------------------

scalastyleConfig := file(".scalastyle-config.xml")

wartremoverErrors in (Compile, compile) ++= Seq(
  Wart.ArrayEquals,
  Wart.FinalCaseClass,
  Wart.OptionPartial,
  Wart.TraversableOps,
  Wart.TryPartial
)

// ----------------------------------------------------------------------------
// install
// ----------------------------------------------------------------------------

val prefix = settingKey[String]("Installation prefix.")

val install = taskKey[Unit]("Install to prefix.")

prefix := sys.env.getOrElse("PREFIX", "/usr/local")

install := {
  import java.nio.file.Files
  import java.nio.file.StandardCopyOption._

  val bindir = file(prefix.value) / "bin"
  if (!bindir.exists) bindir.mkdirs()

  val binary = (nativeLink in Compile).value

  val source = binary.toPath
  val target = (bindir / "smash").toPath

  Files.copy(source, target, COPY_ATTRIBUTES, REPLACE_EXISTING)
}

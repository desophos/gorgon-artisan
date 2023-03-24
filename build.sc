import mill._
import mill.scalajslib._
import mill.scalajslib.api._
import mill.scalalib._
import scalafmt._

val scalaV   = "3.2.2"
val scalaJSV = "1.13.0"

val compilerOptions = Seq(
  "-explain",
  "-explain-types",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-no-indent",
  "-Xfatal-warnings",
  "-Xmax-inlines",
  "128",
  "-Ysafe-init",
)

object gorgonartisan extends ScalaJSModule with ScalafmtModule {
  def scalaVersion   = scalaV
  def scalaJSVersion = scalaJSV

  def scalacOptions = compilerOptions

  def moduleKind = T(ModuleKind.CommonJSModule)

  def moduleDeps = Seq(parser, ui)
}

object parser extends ScalaJSModule with ScalafmtModule {
  def scalaVersion   = scalaV
  def scalaJSVersion = scalaJSV

  def catsCoreV       = "2.8.0"
  def catsEffectV     = "3.3.14"
  def shapelessV      = "3.2.0"
  def http4sV         = "0.23.18"
  def http4sDomV      = "0.2.7"
  def circeV          = "0.15.0-M1"
  def circeTaggedAdtV = "0.10.1"
  def refinedV        = "0.10.1"
  def scribeV         = "3.10.4"

  def scalacOptions = compilerOptions

  def moduleKind = T(ModuleKind.CommonJSModule)

  // def jsEnvConfig = T(JsEnvConfig.JsDom())

  def ivyDeps = Agg(
    ivy"org.typelevel::cats-core::$catsCoreV",
    ivy"org.typelevel::cats-effect::$catsEffectV",
    ivy"org.typelevel::shapeless3-deriving::$shapelessV",
    // ivy"org.http4s::http4s-circe::$http4sV",
    ivy"org.http4s::http4s-dsl::$http4sV",
    ivy"org.http4s::http4s-client::$http4sV",
    ivy"org.http4s::http4s-dom::$http4sDomV",
    ivy"io.circe::circe-core::$circeV",
    ivy"io.circe::circe-generic::$circeV",
    ivy"io.circe::circe-parser::$circeV",
    ivy"org.latestbit::circe-tagged-adt-codec::$circeTaggedAdtV",
    ivy"eu.timepit::refined::$refinedV",
    ivy"com.outr::scribe::$scribeV",
    ivy"com.outr::scribe-cats::$scribeV",
  )

  object test extends Tests with TestModule.Munit {
    def munitV           = "1.0.0-M7"
    def munitCatsEffectV = "2.0.0-M3"

    def ivyDeps = Agg(
      ivy"org.scalameta::munit::$munitV",
      ivy"org.scalameta::munit-scalacheck::$munitV",
      ivy"org.typelevel::munit-cats-effect::$munitCatsEffectV",
    )
  }
}

object ui extends ScalaJSModule with ScalafmtModule {
  def scalaVersion   = scalaV
  def scalaJSVersion = scalaJSV

  def scalaJSDomV = "2.4.0"
  def calicoV     = "0.1.1"

  def scalacOptions = compilerOptions

  def moduleKind = T(ModuleKind.CommonJSModule)

  def moduleDeps = Seq(parser)

  def ivyDeps = Agg(
    ivy"org.scala-js::scalajs-dom::$scalaJSDomV",
    ivy"com.armanbilge::calico::$calicoV",
  )
}

package gorgonartisan.test

import scala.concurrent.duration.Duration
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import gorgonartisan.*
import gorgonartisan.given

import cats.data.OptionT
import cats.effect.IO
import cats.implicits.*
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.http4s.dom.FetchClientBuilder
import scribe.*
import scribe.cats.*

class CdnSuite extends CatsEffectSuite {
  override val munitIOTimeout = Duration(180, "s")

  val log = scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Debug))
    .replace()
    .f[IO]

  val client = FetchClientBuilder[IO].create

  test("files are downloaded") {
    Http4sCdn
      .getFiles(client)(
        List(ContentFile.Items, ContentFile.Recipes, ContentFile.Quests)
      )
      .value
      .map(data => assert(data.isDefined))
  }
}

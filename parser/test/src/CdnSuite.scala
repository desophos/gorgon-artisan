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
import org.http4s.client.Client
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

  given Client[IO] = FetchClientBuilder[IO].create

  test("files are downloaded and parsed") {
    for {
      v       <- Http4sCdn.getVersion
      items   <- Http4sCdn.getItems(v)
      recipes <- Http4sCdn.getRecipes(v)
      quests  <- Http4sCdn.getQuests(v)
    } yield {
      assert(items.size > 0)
      assert(recipes.size > 0)
      assert(quests.size > 0)
    }
  }
}

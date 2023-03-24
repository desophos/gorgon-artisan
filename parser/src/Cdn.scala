package gorgonartisan

import scala.annotation.experimental
import scala.collection.mutable.ArraySeq
import scala.util.Failure
import scala.util.Success

// import io.circe.generic.auto.*
import gorgonartisan.ContentFile.getReader

import cats.Show
import cats.Show.Shown
import cats.data.OptionT
import cats.effect.IO
import cats.implicits.*
import org.http4s.Method.GET
import org.http4s.ParseFailure
import org.http4s.ParseResult
import org.http4s.Uri
import org.http4s.client.*
import org.http4s.client.dsl.io.*
import org.http4s.implicits.uri
// import org.http4s.headers.*
import scribe.cats.{io => log}

given Show[ParseFailure] = Show.fromToString

trait Cdn {
  def getFileUrl(version: Int)(file: ContentFile): String

  def getFile(
      version: Int
  )(
      file: ContentFile
  ): OptionT[IO, Map[Content.Id, Processed[Content]]]

  def getFiles(
      files: List[ContentFile]
  ): OptionT[IO, Map[ContentFile, Map[Content.Id, Processed[Content]]]]
}

object Http4sCdn {
  def getFileUrl(version: Int)(file: ContentFile): String =
    show"http://cdn.projectgorgon.com/v${version}/data/${file}.json"

  def getFile(client: Client[IO])(
      version: Int
  )(
      file: ContentFile
  ): OptionT[IO, Map[Content.Id, Processed[Content]]] =
    Uri
      .fromString(getFileUrl(version)(file))
      .toEitherT[IO]
      .leftSemiflatTap(e =>
        log.error(
          show"Failed to construct URI for version `$version` and file `$file` because of ",
          e,
        )
      )
      .toOption
      .semiflatMap(uri => client.expect[String](GET(uri)))
      .map(file.getReader)
      .flatMapF {
        case Right(data) =>
          log.debug(
            show"Successfully parsed data for ${data.size} ${file}"
          ) *> data.some.pure[IO]
        case Left(e) =>
          log.error(show"Failed to parse ${file} file because of ", e)
            *> None.pure[IO]
      }

  def getFiles(client: Client[IO])(
      files: List[ContentFile]
  ): OptionT[IO, Map[ContentFile, Map[Content.Id, Processed[Content]]]] = for {
    v <- OptionT liftF client.expect[String](
      uri"http://client.projectgorgon.com/fileversion.txt"
    )
    _ <- OptionT liftF log.debug(
      show"Successfully fetched game version: ${v}"
    )
    vInt <- OptionT fromOption v.toIntOption
    data <- files traverse getFile(client)(vInt)
    _    <- OptionT liftF log.debug("Successfully parsed all files")
  } yield Map.from(files zip data)
}

// @experimental
class FetchCdn extends Cdn {
  override def getFileUrl(version: Int)(file: ContentFile): String =
    show"http://cdn.projectgorgon.com/v${version}/data/${file}.json"

  override def getFile(
      version: Int
  )(
      file: ContentFile
  ): OptionT[IO, Map[Content.Id, Processed[Content]]] =
    fetchResponseText(getFileUrl(version)(file))
      .map(file.getReader)
      .flatMapF {
        case Right(data) =>
          log.debug(
            show"Successfully parsed data for ${data.size} ${file}"
          ) *> data.some.pure[IO]
        case Left(e) =>
          log.error(show"Failed to parse ${file} file because of ", e)
            *> None.pure[IO]
      }

  override def getFiles(
      files: List[ContentFile]
  ): OptionT[IO, Map[ContentFile, Map[Content.Id, Processed[Content]]]] = for {
    v <- fetchResponseText("http://client.projectgorgon.com/fileversion.txt")
    _ <- OptionT liftF log.debug(
      show"Successfully fetched game version: ${v}"
    )
    vInt <- OptionT fromOption v.toIntOption
    data <- files traverse getFile(vInt)
    _    <- OptionT liftF log.debug("Successfully parsed all files")
  } yield Map.from(files zip data)
}

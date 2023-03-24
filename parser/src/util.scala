package gorgonartisan

import scala.compiletime.erasedValue
import scala.compiletime.summonInline
import scala.deriving.Mirror
import scala.scalajs.js.Promise
import scala.scalajs.js.Thenable

import cats.Show
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import io.circe.ACursor
import io.circe.Json
import io.circe.JsonNumber
import io.circe.JsonObject
import org.scalajs.dom.Fetch
import org.scalajs.dom.Response
import scribe.cats.{io => log}

def debugTrace[A: Show](a: A): IO[A] = log.debug(a.show) *> a.pure

extension [L: Show, R](it: Iterable[Either[L, R]]) {
  def logLeftCollectRight: Iterable[R] =
    it tapEach {
      case Left(left)   => log.error(left.show)
      case Right(right) => right
    } collect { case Right(right) => right }
}

extension [A](p: Thenable[A]) {
  def toIO: IO[A] = IO.fromFuture(IO(p.toFuture))
}

extension (res: Response) {
  def maybeBody[A](f: Response => Promise[A]): IO[Option[A]] =
    if res.ok then f(res).toIO.some.sequence else Option.empty.pure
  def bodyText: IO[Option[String]] =
    res.maybeBody(_.text())
  def bodyJson: IO[Option[Any]] =
    res.maybeBody(_.json())
}

def fetchResponse(url: String) = OptionT.liftF(Fetch.fetch(url).toIO)

def fetchResponseText(url: String) = fetchResponse(url).flatMapF(_.bodyText)
def fetchResponseJson(url: String) = fetchResponse(url).flatMapF(_.bodyJson)

def seqOfOne[A](x: A): Seq[A] = Seq(x)

extension (json: Json) {
  def flattenArray: Json =
    json.withArray(
      Json fromValues _.foldLeft(Vector.empty)((acc, next) =>
        next.fold(
          acc :+ next,
          _ => acc :+ next,
          _ => acc :+ next,
          _ => acc :+ next,
          acc concat _,
          _ => acc :+ next,
        )
      )
    )

  def toOptions2: (Option[String], Option[String]) =
    json.fold(
      (None, None),
      _ => (None, None),
      _ => (None, None),
      s => (Some(s), None),
      v => (v.get(0).flatMap(_.asString), v.get(1).flatMap(_.asString)),
      _ => (None, None),
    )
}

extension (c: ACursor) {
  def wrapInArray: ACursor = c.withFocus(json =>
    json.fold(
      json,
      _ => Json.arr(json),
      _ => Json.arr(json),
      _ => Json.arr(json),
      Json.fromValues,
      _ => Json.arr(json),
    )
  )

  def flattenArray: ACursor = c.withFocus(_.flattenArray)
}

// import scala.quoted.*

// inline def unchanged[
//     FROM <: Content with Product,
//     TO <: Processed[FROM] with Product,
// ](
//     inline from: FROM
// )(using
//     mTo: Mirror.Of[TO],
//     mFrom: Mirror.Of[FROM] {
//       type MirroredElemTypes = mTo.MirroredElemTypes
//     },
// ) = // : ProcessData[FROM, TO] =
//   ${ coerceType('from) }

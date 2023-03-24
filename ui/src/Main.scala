package gorgonartisan

import scala.annotation.experimental
import scala.scalajs.js.Thenable
import scala.scalajs.js.Thenable.Implicits.*

import gorgonartisan.ContentFile

import calico.*
import calico.dsl.Modifier.given
import calico.dsl.given
import calico.dsl.io.*
import calico.dsl.io.given
import calico.given
import calico.syntax.*
import calico.unsafe.given
import cats.data.OptionT
import cats.effect.*
import cats.effect.syntax.all.*
import cats.implicits.*
import cats.implicits.given
import cats.syntax.all.*
import fs2.*
import fs2.concurrent.*
import org.scalajs.dom.*

// implicit def thenable2IO[A](p: Thenable[A]): IO[A] =
//   IO.fromFuture(IO(p.toFuture))

// @experimental
object Main extends IOWebApp {
  val filesToCache = List(ContentFile.Items, ContentFile.Recipes)

  def run(args: List[String]) = for {
    data <- FetchCdn().getFiles(filesToCache)
  } yield {
    println(
      show"Successfully parsed ${data.size} files with ${data.values.map(_.size).mkString(", ")} entries respectively"
    )
    ExitCode(0)
  }

  // val app = SignallingRef[IO].of("placeholder").toResource.flatMap { version =>
  //   label(version.get)
  // }

  override def render = ???
}

// Example

// package calico

// import calico.dsl.io.*
// import calico.syntax.*
// import calico.widget.*
// import cats.effect.*
// import cats.effect.syntax.all.*
// import cats.syntax.all.*
// import fs2.*
// import fs2.concurrent.*
// import monocle.macros.GenLens

// object Example extends IOWebApp:

//   final case class Person(firstName: String, lastName: String, age: Int)
//   final case class TwoPeople(one: Person, two: Person)

//   def render =
//     SignallingRef[IO]
//       .of(TwoPeople(Person("", "", 0), Person("", "", 0)))
//       .toResource
//       .flatMap { persons =>
//         div(
//           div(
//             h3("View"),
//             Widget.view(persons.discrete)
//           ),
//           div(
//             h3("Edit 1"),
//             Widget.edit(persons.zoom(GenLens[TwoPeople](_.one)))
//           ),
//           div(
//             h3("Edit 2"),
//             Widget.edit(persons.zoom(GenLens[TwoPeople](_.two)))
//           )
//         )
//       }

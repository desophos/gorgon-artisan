package gorgonartisan

// import io.circe.generic.semiauto.*
// import io.circe.Decoder

// given Decoder[Item]   = deriveDecoder
// given Decoder[Recipe] = deriveDecoder

import cats.syntax.functor._
import io.circe.{Decoder, Encoder}, io.circe.generic.auto._
import io.circe.syntax._

// given Encoder[Event] = Encoder.instance {
//   case foo @ Foo(_) => foo.asJson
//   case bar @ Bar(_) => bar.asJson
//   case baz @ Baz(_) => baz.asJson
//   case qux @ Qux(_) => qux.asJson
// }

given Decoder[Content] =
  List[Decoder[Content]](
    Decoder[Item].widen,
    Decoder[Recipe].widen,
    Decoder[Quest].widen
  ).reduceLeft(_ or _)

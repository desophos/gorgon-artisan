package gorgonartisan.test

import gorgonartisan.*

import cats.effect.IO
import munit.CatsEffectSuite

class UtilSuite extends CatsEffectSuite {
  test("fetchResponseText successfully retrieves response from Google") {
    fetchResponseText("http://google.com").value.map(text =>
      assert(text.isDefined)
    )
  }
}

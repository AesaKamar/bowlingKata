package example

import cats._
import cats.implicits._
import cats.instances._
import cats.syntax._

import org.scalatest.{FreeSpec, Matchers}

class ValidationTest extends FreeSpec with Matchers{
  "Validating a game" - {
    "[X,X,X], [X] => INVALID" in {
      Validation.validate(List(FB_Special(Strike, Strike, Strike),
        FN_Strike(Strike))) shouldBe 'invalid
    }
    "11* [X] => INVALID" in {
      Validation.validate(List.fill(11)(FN_Strike(Strike))) shouldBe 'invalid
    }

    "9*[X], [X,X,X] => VALID" in {
      Validation.validate(
        List.fill(9)(FN_Strike(Strike)) ++ List(
          FB_Special(Strike, Strike, Strike))) shouldBe 'valid
    }
  }
}

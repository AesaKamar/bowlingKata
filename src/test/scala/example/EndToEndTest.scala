package example

import cats._
import cats.implicits._
import cats.data.Validated._
import cats.data._
import org.scalatest.{FreeSpec, Matchers}

class EndToEndTest extends FreeSpec with Matchers {

  def composedOperation(inp: String) = {
    Valid(inp)
      .andThen(x => Parsing.parseIntoNFrames(x, 9))
      .andThen(x => Validation.validate(x))
      .andThen(x => Valid(Scoring.score(x)))
  }

  "X X X X X X X X X X X X => 300" in {
    composedOperation("X X X X X X X X X X X X") shouldBe Valid(300)
  }
  "9- 9- 9- 9- 9- 9- 9- 9- 9- 9- => 90" in {
    composedOperation("9- 9- 9- 9- 9- 9- 9- 9- 9- 9-") shouldBe Valid(90)
  }

  "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5 => 150" in {
    composedOperation("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5") shouldBe Valid(150)
  }
}

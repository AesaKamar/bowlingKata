package example

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.OptionValues._
import fastparse._

class ParsingTest extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks{

  "Parsing into normal frames" - {
    "11" in {
      Parsing.parseFrames("11").value shouldBe List(FN_Point(Point(1), Point(1)))
    }
  }

}

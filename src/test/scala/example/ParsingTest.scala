package example

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.OptionValues._
import fastparse._
import fastparse.core.Parsed.{Failure, Success}


class ParsingTest
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  "Testing individual parsers" - {
    import Parsing._
    "11" in {
      pairParser.parse("11") shouldBe Success((Point(1), Point(1)), 2)
    }
    "1-" in {
      pairParser.parse("1-") shouldBe Success((Point(1), Point(0)), 2)
    }
    "1/" in {
      pairParser.parse("1/") shouldBe Success((Point(1), Spare), 2)
    }
    "Normal Frame" - {
      "11" in {
        normalFrameParser.parse("11") shouldBe Success(FN_Point(Point(1), Point(1)), 2)
      }
      "1/" in {
        normalFrameParser.parse("1/") shouldBe Success(FN_Spare(Point(1), Spare), 2)
      }
      "X" in {
        normalFrameParser.parse("X") shouldBe Success(FN_Strike(Strike), 1)
      }
    }
    "Bonus Frame" - {
      "11" in {
        bonusFrameParser.parse("11") shouldBe Success(FB_Normal(Point(1), Point(1)), 2)
      }
      "1/X" in {
        bonusFrameParser.parse("1/X") shouldBe Success(FB_Special(Point(1), Spare, Strike), 3)
      }
      "XXX" in {
        bonusFrameParser.parse("XXX") shouldBe Success(FB_Special(Strike, Strike, Strike), 3)
      }
    }

  }

  "Parsing into normal frames" ignore {
    "Pointed" - {
    }
  }

}

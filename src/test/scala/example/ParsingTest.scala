package example

import cats.data.Validated._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.OptionValues._
import fastparse._
import fastparse.core.Parsed.{Failure, Success}
import ParsingTest._
import org.scalacheck._

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

  "Parsing into normal frames" - {
    "[] => VALID" in {
      Parsing.parseIntoNFrames("   ", 0) shouldBe 'valid
    }
    "11 11 11 11 11 => VALID" in {
      Parsing.parseIntoNFrames("11 11 11 11 11", 4) shouldBe Valid(List.fill(4)(FN_Point(Point(1), Point(1))) ++
        List(FB_Normal(Point(1), Point(1))))
    }
    "X => VALID" in {
      Parsing.parseIntoNFrames("X", 1) shouldBe Valid(
        List(FN_Strike(Strike)))
    }
    "X X X => VALID" in {
      Parsing.parseIntoNFrames("X X X ", 3) shouldBe Valid(
        List(FN_Strike(Strike), FN_Strike(Strike), FN_Strike(Strike)))
    }
    "XXX => VALID" in {
      Parsing.parseIntoNFrames("XXX", 0) shouldBe Valid(
        List(FB_Special(Strike, Strike, Strike)))
    }
    "X X X X X X X X X XXX => VALID" in {
      Parsing.parseIntoNFrames("X X X X X X X X X XXX", 9) shouldBe Valid(
        List.fill(9)(FN_Strike(Strike)) ++ List(FB_Special(Strike, Strike, Strike)))
    }
    "5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5  => VALID" in {
      Parsing.parseIntoNFrames("5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5", 9) shouldBe Valid(
        List.fill(9)(FN_Spare(Point(5), Spare)) ++ List(FB_Special(Point(5), Spare, Point(5))))
    }


  }
  "Property Based "-{
    "all normal games should parse" in {
      forAll(genListOf9Frames){x =>
        val str = x.map(pairToNormalFrameString).mkString
        Parsing.parseIntoNFrames( str , 9) shouldBe 'valid
      }
    }
  }

}

object ParsingTest{

  val genFirst2Numbers = for{
    first <- Gen.oneOf(0 to 10)
  second <- Gen.oneOf(0 to (10-first))
}yield(first, second)

val genListOf9Frames = Gen.listOfN(9, genFirst2Numbers)


  def nToDash(n: Int): String =
    if (n ==0) "-"
    else s"$n"

  def pairToNormalFrameString(p: (Int, Int)) = {
    p match{
      case (10, _) => "X"
      case (i, j)  if (i + j) == 10 => s"${nToDash(i)}/"
      case (i, j) => s"${nToDash(i)}${nToDash(j)}"
    }
  }
}
package example

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class ScoringTest
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  val scoringFunction = Scoring.score _

  "Scoring Pointed values" - {
    "[0,0] => 0" in {
      scoringFunction(FN_Point(Point(0), Point(0)) :: Nil) shouldBe 0
    }
    "[0,0] , [0,0] , [0,0]=> 0" in {
      scoringFunction(
        List(FN_Point(Point(0), Point(0)),
             FN_Point(Point(0), Point(0)),
             FN_Point(Point(0), Point(0)))) shouldBe 0
    }
    "10*[1,1] , [1,1, ] => 20" in {
      scoringFunction(
        List.fill(9)(FN_Point(Point(1), Point(1))) ++ List(
          FB_Normal(Point(1), Point(1)))
      ) shouldBe 20
    }
  }

}

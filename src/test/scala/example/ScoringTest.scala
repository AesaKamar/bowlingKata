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
      scoringFunction(F_Point(Point(0), Point(0)) :: Nil) shouldBe 0
    }
    "[0,0] , [0,0] => 0" in {
      scoringFunction(
        F_Point(Point(0), Point(0)) :: F_Point(Point(0), Point(0)) :: Nil) shouldBe 0
    }
  }

}

package example

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import cats._
import cats.data.Validated._
import cats.data._
import cats.implicits._
import cats.syntax._

class ScoringTest
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks {

  val scoringFunction = Scoring.score _

  "Helpers" - {
    "flatten" in {
      Scoring.flatten(List(FN_Spare(Point(0), Spare),
                           FB_Special(Strike, Strike, Strike))) shouldBe List(
        Point(0),
        Spare,
        Strike,
        Strike,
        Strike)
    }
  }

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

    "Scoring for spares" - {
      "[5,/] , [5,/], [0,0] => 25" in {
        scoringFunction(
          List(FN_Spare(Point(0), Spare),
               FN_Spare(Point(0), Spare),
               FN_Point(Point(0), Point(0)))
        )
      }

      "9* [0,0] , [5,/,5] => 15" in {
        scoringFunction(
          List.fill(9)(FN_Point(Point(0), Point(0))) ++ List(
            FB_Special(Point(5), Spare, Point(5)))
        ) shouldBe 15
      }
      "9* [5,/] , [5,/,5] => 150" in {
        scoringFunction(
          List.fill(9)(FN_Spare(Point(5), Spare)) ++ List(
            FB_Special(Point(5), Spare, Point(5)))
        ) shouldBe 150
      }
    }
    "Scoring for strikes" - {
      "[X], [X], [X] , [0,0]=> 60" in {
        scoringFunction(List.fill(3)(FN_Strike(Strike)) ++ List(
          FN_Point(Point(0), Point(0)))) shouldBe 60
      }
      "9*[X], [X,X,X]=> 300" in {
        scoringFunction(
          List.fill(9)(FN_Strike(Strike)) ++ List(
            FB_Special(Strike, Strike, Strike))) shouldBe 300
      }
    }
  }

  "Validating a game" - {
    "[X,X,X], [X] => INVALID" in {
      Scoring.validate(List(FB_Special(Strike, Strike, Strike),
                            FN_Strike(Strike))) shouldBe 'invalid
    }
    "11* [X] => INVALID" in {
      Scoring.validate(List.fill(11)(FN_Strike(Strike))) shouldBe 'invalid
    }

    "9*[X], [X,X,X] => VALID" in {
      Scoring.validate(
        List.fill(9)(FN_Strike(Strike)) ++ List(
          FB_Special(Strike, Strike, Strike))) shouldBe 'valid
    }
  }

}

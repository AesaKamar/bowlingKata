package example

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._
import cats.syntax._

sealed trait Frame

sealed trait NormalFrame extends Frame
case class FN_Strike(b1: Strike.type) extends NormalFrame
case class FN_Spare(b1: Point, b2: Spare.type) extends NormalFrame
case class FN_Point(b1: Point, b2: Point) extends NormalFrame

sealed trait BonusFrame extends Frame
case class FB_Special(b1: Bowl, b2: S_Bowl, b3: Bowl) extends BonusFrame
case class FB_Normal(b1: Point, b2: Point) extends BonusFrame

sealed trait Bowl {
  val v: Int
}
/** Special Bowl of either Strike or Spare */
sealed trait S_Bowl extends Bowl {
  val v = 10
}
case class Point(v: Int) extends Bowl
case object Spare extends S_Bowl
case object Strike extends S_Bowl

object Scoring {

  def sumRaw(bowls: List[Bowl]): Int =
    bowls match {
      case Nil               => 0
      case e :: Nil          => e.v
      case _ :: Spare :: Nil => 10
      case h :: t            => h.v + sumRaw(t)
    }

  def flatten(frames: List[Frame]): List[Bowl] = {
    frames.foldRight[List[Bowl]](Nil)((frm, tail) => {
      val toAdd: List[Bowl] = frm match {
        case FN_Strike(b1)          => List(b1)
        case FN_Spare(b1, b2)       => List(b1, b2)
        case FN_Point(b1, b2)       => List(b1, b2)
        case FB_Special(b1, b2, b3) => List(b1, b2, b3)
        case FB_Normal(b1, b2)      => List(b1, b2)
        case _                      => List()
      }
      toAdd ++ tail
    })
  }

  def takeNBowls(frames: List[Frame])(n: Int): List[Bowl] =
    flatten(frames).take(n)

  def score(game: List[Frame]): Int = {
    game match {
      case Nil => 0
      //Handle Bonus Frames
      case FB_Normal(b1, b2) :: tail        => b1.v + b2.v + score(tail)
      case FB_Special(_, Spare, b3) :: tail => Spare.v + b3.v + score(tail)
      case FB_Special(b1, Strike, b3) :: tail =>
        b1.v + Strike.v + b3.v + score(tail)
      // Handle Normal Frames
      case FN_Point(b1, b2) :: tail => b1.v + b2.v + score(tail)
      case FN_Spare(b1, Spare) :: tail =>
        Spare.v + sumRaw(takeNBowls(tail)(1)) + score(tail)
      case FN_Strike(Strike) :: tail =>
        Strike.v + sumRaw(takeNBowls(tail)(2)) + score(tail)
    }
  }

  def validate(game: List[Frame])
    : ValidatedNel[BowlingValidationFailure, List[Frame]] = {
    //A bonus frame may only be the last frame
    val validation_BonusFoundIntemediately = {
      val hasNoIntermediateBonus = game.take(game.length - 1).forall {
        case n: NormalFrame => true
        case _              => false
      }
      if (hasNoIntermediateBonus) Valid(game)
      else Invalid(NonEmptyList.of(BonusFoundIntermediately))
    }

    //A Game should not be more than 10 frames
    val validation_GameTooLong = {
      if (game.length > 10) Invalid(NonEmptyList.of(GameTooLong))
      else Valid(game)
    }

    validation_BonusFoundIntemediately.combine(validation_GameTooLong)
  }
}
sealed trait BowlingValidationFailure
case object BonusFoundIntermediately extends BowlingValidationFailure
case object GameTooLong extends BowlingValidationFailure

package example

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._
import cats.syntax._


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


}

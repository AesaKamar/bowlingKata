package example

sealed trait Frame
sealed trait NormalFrame extends Frame

case class FN_Strike(b1: Strike.type ) extends NormalFrame
case class FN_Spare(b1: Point, b2: Spare.type ) extends NormalFrame
case class FN_Point(b1: Point, b2: Point ) extends NormalFrame

sealed trait BonusFrame extends Frame
case class FB_Special(b1: Bowl, b2: S_Bowl, b3: Bowl) extends BonusFrame
case class FB_Normal(b1: Point, b2: Point) extends BonusFrame


sealed trait Bowl{
  val v : Int
}
sealed trait S_Bowl extends Bowl{
  val v = 10
}
case class Point(v: Int) extends Bowl
case object Spare extends S_Bowl
case object Strike extends S_Bowl


object Scoring {

  def score(game: List[Frame]): Int = {
    game match {
      case Nil => 0
        //Handle Bonus Frames
      case FB_Normal(b1, b2) :: Nil => b1.v + b2.v
        // Handle Normal Frames
      case FN_Point(b1, b2) :: tail => b1.v + b2.v + score(tail)
    }
  }
}


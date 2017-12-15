package example

sealed trait Frame
sealed trait NormalFrame extends Frame

case class F_Strike(b1: Strike.type ) extends NormalFrame
case class F_Spare (b1: Point, b2: Spare.type ) extends NormalFrame
case class F_Point (b1: Point, b2: Point ) extends NormalFrame

case class FBonus(b1: Bowl, b2: Bowl, b3: Option[Bowl]) extends Frame


sealed trait Bowl{
  val v : Int
}
case class Point(v: Int) extends Bowl
case object Spare extends Bowl{
  val v = 10
}
case object Strike extends Bowl{
  val v = 10
}


object Scoring {

  def score(game: List[Frame]): Int = {
    game match {
      case head :: tail => 0
      case Nil => 0
    }
  }
}


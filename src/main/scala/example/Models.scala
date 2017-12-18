package example

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




sealed trait BowlingValidationFailure
case object BonusFoundIntermediately extends BowlingValidationFailure
case object GameTooLong extends BowlingValidationFailure
case object ParsingError extends BowlingValidationFailure
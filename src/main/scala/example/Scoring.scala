package example

sealed trait Frame
case class F2(b1: Bowl, b2: Bowl) extends Frame
case class FB(b1: Bowl, b2: Bowl, b3: Option[Bowl])


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

}


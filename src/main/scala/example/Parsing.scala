package example

import cats._
import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}
import pprint._

object Parsing {
  val validChar = P(CharIn("-123456789X/"))

  val pointParser = P(CharIn("-123456789").!).map {
    case "-" => Point(0)
    case p   => Point(p.toInt)
  }
  val strikeParser = P("X").map(_ => Strike)
  val spareParser = P("/").map(_ => Spare)

  val specialBowlParser : Parser[S_Bowl] =  strikeParser | spareParser
  val bowlParser : Parser[Bowl] = pointParser | specialBowlParser


  val pairParser : Parser[(Point, Bowl)]= pointParser ~ (pointParser | spareParser)

  // This is a hack because scala does not implement Sum types for ADTs nicely, so type inference breaks down.
  // Possibly fixable by using Shapeless Generic[] to convert the tuples to case class instances
  val normalFrameParser: Parser[NormalFrame] = (pairParser | strikeParser).map{
    case (p1 : Point, Spare) => FN_Spare(p1, Spare)
    case (p1: Point, p2: Point) => FN_Point(p1, p2)
    case Strike => FN_Strike(Strike)
  }

  val bonusFrameParser : Parser[BonusFrame] =
    ((pointParser ~ pointParser) | (bowlParser ~ specialBowlParser ~ bowlParser)).map{
    case (p1: Point, p2: Point) => FB_Normal(p1, p2)
    case (b1: Bowl, sb2: S_Bowl, b3: Bowl) => FB_Special(b1, sb2, b3)
  }

  val frameParser : Parser[Frame] = normalFrameParser | bonusFrameParser

  val framesParser : Parser[List[Frame]] = frameParser.rep.map(_.toList)


  def parseIntoFrames(inp: String): Validated[ParsingError.type , List[Frame]] = {
    ???
  }

}



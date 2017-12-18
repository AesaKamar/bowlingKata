package example

import cats._
import cats.implicits._
import fastparse.all._
import fastparse.core.Parsed.{Failure, Success}
import pprint._

object Parsing {
  val validChar = P(CharIn("-123456789X/"))

  val pointParser = P(CharIn("-123456789").!).map(p => Point(p.toInt))
  val strikeParser = P("X")
  val spareParser = P("/")

  def parseFrames(input: String): Option[List[Frame]] = {

    val inputSansWhitespace = input.filterNot(_.isWhitespace)
    val res = P(validChar.! ~ validChar.! ~ validChar.?.!).rep.parse(inputSansWhitespace)

    res match {
      case Success(frames, _) =>
        frames.toList.traverse(parseFrame)
      case Failure(_, _, _) => None
    }
  }

  def parseFrame(attempt: (String, String, String)) : Option[Frame] = {
    attempt match{
        //This is the case of a Normal Frame
      case (b1, b2, "") => {
        (pointParser.parse(b1), pointParser.parse(b2)) match {
            case (Success(p1, _), Success(p2, _)) => Some(FN_Point(p1, p2))
          }


      }
        //This is the case of a Bonus Frame
      case (b1, b2, b3) => {

        ???
      }
    }

  }
}

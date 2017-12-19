package example

import cats._
import cats.implicits._
import cats.instances._
import cats.syntax._
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}

object Validation {
  def validate(game: List[Frame])
  : ValidatedNel[BowlingValidationFailure, List[Frame]] = {
    //A bonus frame may only be the last frame
    val validation_BonusFoundIntemediately : ValidatedNel[BowlingValidationFailure, List[Frame]]= {
      val hasNoIntermediateBonus = game.take(game.length - 1).forall {
        case n: NormalFrame => true
        case _              => false
      }
      if (hasNoIntermediateBonus) Valid(game)
      else Invalid(NonEmptyList.of(BonusFoundIntermediately))
    }

    //A Game should not be more than 10 frames
    val validation_GameTooLong : ValidatedNel[BowlingValidationFailure, List[Frame]] = {
      if (game.length > 10) Invalid(NonEmptyList.of(GameTooLong))
      else Valid(game)
    }

    (validation_BonusFoundIntemediately |@| validation_GameTooLong).map{(_, _) => game}
  }
}



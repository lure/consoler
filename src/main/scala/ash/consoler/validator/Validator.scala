package ash.consoler.validator

import ash.consoler.consts.{StringSource, StubStringSource}
import ash.consoler.model._

/**
  * Validator trait. Instances of validator designated to use everywhere across application.
  *
  * User: Shubert Alexandr
  * Date: 25.11.2016
  *
  * @tparam T generic type of value to validate
  */
trait Validator[T] {
  /**
    * Requires coordinates to exist in coordinate space of canvas. This implementation consider valid max values to be
    * equal of canvas height or width, as user imput is 1-based.
    *
    * @param value  fill command to be checked
    * @param holder used in range checks
    * @return true if valid
    */
  def validate(value: T, holder: CanvasHolder)(implicit res: StringSource): Either[String, T]

  protected def defaultPointValidation(x: Int, y: Int, holder: CanvasHolder): Boolean = {
    x > 0 && y > 0 && x <= holder.width && y <= holder.height
  }
}

/**
  * declaration interface to make validator entry point replaceable
  */
trait ValidationCenter {
  def performValidation[T >: Command](cmd: T, holder: CanvasHolder): Either[String, T]
}

object ValidationCenterImpl extends ValidationCenter{
  implicit val resources = StubStringSource
  implicit val lineValidator = new LineValidator
  implicit val rectangleValidator = new RectangleValidator
  implicit val bucketFillValidator = new BucketFillValidator
  implicit val canvasValidator = new CanvasValidator

  /**
    * Performs validation basing on command type. If no validator found, command itself returns. That may be
    * not the best solution, but saves from impelementing noop validator instances.
    * @param cmd command to be checked
    * @param holder canvas holder. Required to check bounds
    * @tparam T command type
    * @return either command or error message
    */
  def performValidation[T >: Command](cmd: T, holder: CanvasHolder): Either[String, T] = {
    cmd match {
      case c: Line => lineValidator.validate(c, holder)
      case c: Rectangle => rectangleValidator.validate(c, holder)
      case c: BucketFill => bucketFillValidator.validate(c, holder)
      case c: Canvas => canvasValidator.validate(c, holder)
      case _ => Right(cmd)
    }
  }
}

class CanvasValidator extends Validator[Canvas] {
  /**
    * Requires coordinates to exist in coordinate space of canvas. This implementation consider valid max values to be
    * equal of canvas height or width, as user input is 1-based.
    *
    * @param value  fill command to be checked
    * @param holder used in range checks
    * @return true if valid
    */
  override def validate(value: Canvas, holder: CanvasHolder)(implicit res: StringSource): Either[String, Canvas] = {
    if (value.x > 0 && value.y > 0 && value.x < 32767 && value.y < 32767){
      Right(value)
    } else {
      Left(res.coordMustBeBetween)
    }
  }
}

class LineValidator extends Validator[Line] {

  /**
    * @inheritdoc
    * In addition, line must be strictly horizontal or vertical
    */
  override def validate(value: Line, holder: CanvasHolder)(implicit res: StringSource): Either[String, Line] = {
    if (value.x1 != value.x2 && value.y1 != value.y2) {
      Left(res.neitherHorizonOrVertical)
    } else {
      if (!defaultPointValidation(value.x1, value.y1, holder)) {
        Left(res.invalidPoint(value.x1, value.y1))
      } else {
        if (!defaultPointValidation(value.x2, value.y2, holder)) {
          Left(res.invalidPoint(value.x2, value.y2))
        } else {
          Right(value)
        }
      }
    }
  }
}

class RectangleValidator extends Validator[Rectangle] {
  /**
    * @inheritdoc
    */
  override def validate(value: Rectangle, holder: CanvasHolder)(implicit res: StringSource): Either[String, Rectangle] = {
    if (!defaultPointValidation(value.x1, value.y1, holder)) {
      Left(res.invalidPoint(value.x1, value.y1))
    } else {
      if (!defaultPointValidation(value.x2, value.y2, holder)) {
        Left(res.invalidPoint(value.x2, value.y2))
      } else {
        Right(value)
      }
    }
  }
}

class BucketFillValidator extends Validator[BucketFill] {
  // Everything less than is control or unprintable
  val ASCII_LITERALS_START = 0x20

  /**
    * this implementation requires coordinates to be placed in coordinate space of canvas
    * and color to be a printable character.
    *
    * @param value  fill command to be checked
    * @param holder used in range checks
    * @return true if valid
    */
  override def validate(value: BucketFill, holder: CanvasHolder)(implicit res: StringSource): Either[String, BucketFill] = {
    if (validateColor(value.color, holder)) {
      if (!defaultPointValidation(value.x, value.y, holder)) {
        Left(res.invalidPoint(value.x, value.y))
      } else {
        Right(value)
      }
    } else {
      Left(res.invalidColor(value.color))
    }
  }

  /**
    * Checks whether character is not control, undefined of special, i.e. it can be printed.
    * The failback to character instead of codepoints to support legacy consoles.
    *
    * @param char character (not codepoint)
    * @return true if character is considered `printable`, false otherwise
    */
  def validateColor(char: Char, holder: CanvasHolder)(implicit res: StringSource): Boolean = {
    val block = Option(Character.UnicodeBlock.of(char))
    !(char < ASCII_LITERALS_START || Character.isISOControl(char) || block.contains(Character.UnicodeBlock.SPECIALS))
  }
}
package ash.consoler.controller.command

import ash.consoler.consts.StringSource
import ash.consoler.model._
import shapeless.{::, HList, HNil, Poly1}

import scala.util.{Failure, Success, Try}

/**
  * Command basis. Provides simple extensible framework for command parse building.
  *
  * User: Shubert Alexandr
  * Date: 25.11.2016
  */
trait CommandParser[+T] {
  def apply(params: List[String]): Either[String, T]
}

//* TODO: 1 tech debt eliminate case xs => Left(xs.map(EitherFolder).mkString("Line ", ", ", ""))  reflection+impl
//* TODO: 2 shorten pattern matchings in parseInt, parseChar
//* TODO: 3 use resources to make error messages


trait BaseCommandParser[T] extends CommandParser[T] {

  import shapeless.ops.hlist.Reverse

  // Interface part
  type SequenceType = ((Seq[String], HList)) => (Seq[String], HList)

  def sequence: SequenceType

  // params types list BEFORE reverse
  type L <: HList
  // params types list AFTER reverse. Strictly said, this is not required, but allows to reduce code duplication.
  type R <: HList

  // magnet required Reverse
  def nailLToR[L <: HList](implicit rev: Reverse[L]): Reverse[L] {type Out = rev.Out} = rev

  // We want to use map on HList and therefore must provide appropriate converters
  object EitherFolder extends Poly1 {
    implicit def caseSEither: Case[Either[String, Int]] =
      at[Either[String, Int]] { e => e.fold(s => s, a => a.toString) }

    implicit def caseCEither: Case[Either[String, Char]] =
      at[Either[String, Char]] { e => e.fold(s => s, a => a.toString) }
  }

  protected def parseInt(param: String)(conveyor: (Seq[String], HList)): (Seq[String], HList) = {
    import scala.collection.immutable.::
    val result = conveyor._1 match {
      case Nil => (Nil, Left(param + " not found"))
      case "" :: _ => (Nil, Left(param + " not found"))
      case x :: xs => Try {
        val i = Integer.valueOf(x)
        if (i > Short.MaxValue) throw new IllegalArgumentException("Value out of range")
        i
      } match {
        case Success(i) => (xs, Right(i))
        case Failure(e: IllegalArgumentException) if e.getMessage.contains("Value out of range") =>
          (xs, Left(s"$param: [$x] must be between 0 and ${Short.MaxValue}"))
        case _ => (xs, Left(s"$param: [$x] invalid value"))
      }
    }
    result.copy(_2 = result._2 :: conveyor._2)
  }

  protected def parseStub(param: String)(conveyor: (Seq[String], HList)): (Seq[String], HList) = {
    (Nil, HNil)
  }

  protected def parseChar(param: String)(conveyor: (Seq[String], HList)): (Seq[String], HList) = {
    import scala.collection.immutable.::
    val result = conveyor._1 match {
      case Nil => (Nil, Left(param + " not found"))
      case "" :: _ => (Nil, Left(param + " not found"))
      case x :: xs => if (x.length == 1) {
        (xs, Right(x.charAt(0)))
      } else {
        (xs, Left(s"$param: [$x] one character expected"))
      }
    }
    result.copy(_2 = result._2 :: conveyor._2)
  }

  /**
    * This abstract function exists for the solely purpose to be applied at the point where
    * heterogeneous types known and hence could be proven.
    * Please, use `override def reverseParams(params: L): R = params.reverse` implementation
    * if only you have strong motivation to re-implement in your own way
    *
    * @param params HList of parsed command line parameters.
    * @return reversed list
    */
  def reverseParams(params: L): R

  /**
    * Maps correct command sequence to Command representation. Otherwise must build error message.
    * Sadly, compiler can't prove that 'default' branch type list may be mapped by PolyFolder, so it is required
    * to provide default branch in every implementation by analogue with `.reverse` call.
    *
    * @return Command instance if defined.
    */
  def handleParams: PartialFunction[R, Either[String, T]]

  /**
    * Performs command parsing.
    *
    * @param params space-free command line params
    * @return either command or error message
    */
  def apply(params: List[String]): Either[String, T] = {
    val lst = sequence(params, HNil)._2.asInstanceOf[L]
    val reversedList = reverseParams(lst)
    handleParams(reversedList)
  }
}

/**
  * Intcut to define palindrome list commands, i.e. `implicitly[L =:= R]`
  * Example of such command is Canvas, it's parameter list is `Int, Int`.
  */
trait MirroredCommandParser[T] extends BaseCommandParser[T] {
  override type R = L
}

class BucketParser extends BaseCommandParser[BucketFill] {
  val sequence: SequenceType = parseInt("x") _ andThen parseInt("y") andThen parseChar("color")
  override type L = Either[String, Char] :: Either[String, Int] :: Either[String, Int] :: HNil
  override type R = Either[String, Int] :: Either[String, Int] :: Either[String, Char] :: HNil

  override def reverseParams(params: L): R = params.reverse

  override def handleParams: PartialFunction[R, Either[String, BucketFill]] = {
    case Right(x) :: Right(y) :: Right(c) :: HNil => Right(BucketFill(x, y, c))
    case xs => Left(xs.map(EitherFolder).mkString("BucketFill ", ", ", ""))
  }
}

class LineParser extends MirroredCommandParser[Line] {
  val sequence: SequenceType = parseInt("x1") _ andThen parseInt("y1") andThen parseInt("x2") andThen parseInt("y2")
  override type L = Either[String, Int] :: Either[String, Int] :: Either[String, Int] :: Either[String, Int] :: HNil

  override def reverseParams(params: L): R = params.reverse

  override def handleParams: PartialFunction[R, Either[String, Line]] = {
    case Right(x1) :: Right(y1) :: Right(x2) :: Right(y2) :: HNil => Right(Line(x1, y1, x2, y2))
    case xs => Left(xs.map(EitherFolder).mkString("Line ", ", ", ""))
  }
}

class RectangleParser extends MirroredCommandParser[Rectangle] {
  val sequence: SequenceType = parseInt("x1") _ andThen parseInt("y1") andThen parseInt("x2") andThen parseInt("y2")
  override type L = Either[String, Int] :: Either[String, Int] :: Either[String, Int] :: Either[String, Int] :: HNil

  override def reverseParams(params: L): R = params.reverse

  override def handleParams: PartialFunction[R, Either[String, Rectangle]] = {
    case Right(x1) :: Right(y1) :: Right(x2) :: Right(y2) :: HNil => Right(Rectangle(x1, y1, x2, y2))
    case xs => Left(xs.map(EitherFolder).mkString("Rectangle ", ", ", ""))
  }
}

class CanvasParser extends MirroredCommandParser[Canvas] {
  val sequence: SequenceType = parseInt("x") _ andThen parseInt("y")
  override type L = Either[String, Int] :: Either[String, Int] :: HNil

  override def reverseParams(params: L): R = params.reverse

  override def handleParams: PartialFunction[R, Either[String, Canvas]] = {
    case Right(x) :: Right(y) :: HNil => Right(Canvas(x, y))
    case xs => Left(xs.map(EitherFolder).mkString("Canvas ", ", ", ""))
  }
}

/**
  * Sometimes we need a command which not a command exactly but some sort of token.
  * There is nothing to parse and nothing to care about except error/other message.
  *
  * @tparam T command type or Nothing.
  */
abstract class SyntheticCommand[T] extends MirroredCommandParser[T] {
  override def sequence = parseStub("")

  override type L = HNil

  override def reverseParams(params: L): R = params
}

class QuitParser extends SyntheticCommand[Quit.type] {
  override def handleParams: PartialFunction[R, Either[String, Quit.type]] = {
    case _ => Right(Quit)
  }
}

class EmptyParser(implicit source: StringSource) extends SyntheticCommand[Nothing] {
  override def handleParams: PartialFunction[R, Either[String, Nothing]] = {
    case _ => source.emptyCommandMessage
  }
}

class UnknownParser(cmd: String)(implicit source: StringSource) extends SyntheticCommand[Nothing] {
  override def handleParams: PartialFunction[R, Either[String, Nothing]] = {
    case _ => source.unknownCommand(cmd)
  }
}

class ClearParser extends SyntheticCommand[Clear.type] {
  override def handleParams: PartialFunction[R, Either[String, Clear.type]] = {
    case _ => Right(Clear)
  }
}
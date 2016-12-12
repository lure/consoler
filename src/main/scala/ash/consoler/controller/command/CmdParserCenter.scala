package ash.consoler.controller.command

import ash.consoler.consts.StubStringSource
import ash.consoler.model._

/**
  * Declaration trait. By implementing this trait it is possible to extend functionality.
  *
  * User: Shubert Alexandr 
  * Date: 25.11.2016
  * Description
  */
trait CmdParserCenter {
  /**
    * Parses single param line split to string array to command.
    * Implementations must take care of any possible empty strings and spaces
    *
    * @param cmds single param line split to string array
    * @return either Right command or Left error message
    */
  def parse(cmds: String): Either[String, Command]
}

class CmdParserCenterImpl extends CmdParserCenter {

  implicit val stringSource = StubStringSource
  protected val parseMap: Map[String, CommandParser[Command]] = Map(
    "L" -> new LineParser,
    "R" -> new RectangleParser,
    "B" -> new BucketParser,
    "C" -> new CanvasParser,
    "Q" -> new QuitParser,
    "" -> new EmptyParser
  ).withDefault(new UnknownParser(_))

  def parse(cmds: String): Either[String, Command] = {
    val cleanList = if (cmds == null) {
      Nil
    } else {
      val split = cmds.split("\\s+").toList
      split.filter(_.trim != "")
    }
    val sequence = additionalParse orElse defaultParse
    sequence(cleanList)
  }

  protected def defaultParse: PartialFunction[List[String], Either[String, Command]] = {
    case x :: xs => parseMap(x.toUpperCase)(xs)
    case _ => parseMap("")(Nil)
  }

  /**
    * Override this to add additional checks
    *
    * @return partial function to apply BEFORE default behaviour.
    */
  def additionalParse: PartialFunction[List[String], Either[String, Command]] = PartialFunction.empty
}